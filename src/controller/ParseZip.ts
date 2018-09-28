// A class for parsing zip file

import {InsightError} from "./IInsightFacade";
import {Course} from "./Course";
import {Section} from "./Section";
import Log from "../Util";
import JSZip = require("jszip");

export class ParseZip {
    private zip: any;

    public constructor() {
        this.zip = new JSZip();
    }

    public parseZip(content: string): Promise<Course[]> {
        Log.info("in parseZip");
        let t = this;
        let arrayOfPromises: Array<Promise<any>> = [];
        return new Promise(function (fulfill, reject) {
            t.zip.loadAsync(content, {base64: true})
                .then(function (dataInZip: JSZip) {
                    for (let courseName in dataInZip.files) {
                        if (courseName.match("courses/")) {
                            let coursePromise = t.stringifyAndParse(courseName);
                            arrayOfPromises.push(coursePromise);
                        }
                    }
                    Promise.all(arrayOfPromises).then(function (courses: Course[]) {
                        Log.info("promise all done");
                        let result: Course[] = [];
                        for (let c of courses) {
                            if (c !== null) {
                                result.push(c);
                            }
                        }
                        if (result.length === 0) {
                            reject (new InsightError());
                        } else {
                            fulfill(result);
                        }
                    }).catch(function () {
                        reject(new InsightError());
                    });
                }).catch(function () {
                    reject(new InsightError());
            });
        });
    }

    private stringifyAndParse(courseName: string): Promise<Course> {
        let t = this;
        if (t.zip.file(courseName) === null) {
            return null;
        } else {
            return new Promise(function (fulfill) {
                t.zip.file(courseName).async("text")        // Stringify a single course
                    .then(function (courseData: any) {
                        let json;
                        try {
                            json = JSON.parse(courseData);  // Parse the string into JSON
                        } catch (err) {
                            fulfill(null);   // Non-JSON format course
                        }
                        if (json.result.length === 0) {
                            fulfill(null);   // No-section course
                        } else {
                            let course = new Course(courseName);  // Course with good format and has section
                            for (let section of json.result) {
                                course.addSection(new Section(section.Subject, section.Course,
                                    section.Avg, section.Professor, section.Title, section.Pass,
                                    section.Fail, section.Audit, section.id.toString(), section.Year));
                            }
                            fulfill(course);
                        }
                    }).catch(function () {
                        fulfill(null);       // Cannot stringify the course
                });
            });
        }
    }
}
