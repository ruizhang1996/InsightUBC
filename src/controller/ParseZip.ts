// A class for parsing zip file

import {InsightError} from "./IInsightFacade";
import {Course} from "./Course";
import {Section} from "./Course";
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
                    arrayOfPromises = t.loadFilesInZip(dataInZip);
                    Log.info("in parseZip 2");
                    Promise.all(arrayOfPromises).then(function (courses: Course[]) {
                        Log.info("promise all done");
                        let result: Course[] = [];
                        for (let c of courses) {
                            if (c !== null) {
                                result.push(c);
                            }
                        }
                        Log.trace("in parseZip 3");
                        if (result.length === 0) {
                            reject (new InsightError());
                        } else {
                            fulfill(result);
                        }
                    }).catch(function () {
                        Log.error("err 1 in parseZip");
                        reject(new InsightError());
                    });
                }).catch(function () {
                    Log.error("err 2 in parseZip");
                    reject(new InsightError());
            });
        });
    }

    private loadFilesInZip(dataInZip: JSZip): Array<Promise<Course>> {
        Log.info("in loadFiles In Zip");
        let t = this;
        let arrayOfPromises: Array<Promise<any>> = [];
        for (let path in dataInZip.files) {
            if (path.match("courses/")) {
                let coursePromise = t.getData(path);
                arrayOfPromises.push(coursePromise);
            }
        }
        Log.info("in loadFiles In Zip end");
        return arrayOfPromises;
    }

    private getData(courseName: string): Promise<Course> {
        let t = this;
        if (t.zip.file(courseName) === null) {
            return null;
        } else {
            return new Promise(function (fulfill) {
                t.zip.file(courseName).async("string")
                    .then(function (courseData: any) {
                        let parsedCourse = t.parseCourse(courseName, courseData);
                        if (parsedCourse === null) {
                            fulfill(null);
                        } else {
                            fulfill(parsedCourse);
                        }
                    }).catch(function () {
                        fulfill(null);
                });
            });
        }
    }

    private parseCourse(courseName: string, courseData: string): Course {
        let json;
        try {
            json = JSON.parse(courseData);
        } catch (err) {
            return null;
        }
        if (json.result.length === 0) {
            return null;   // no section course
        } else {
            let course = new Course(courseName);
            for (let section of json.result) {
                course.addSection(new Section(section["Subject"], section["Course"],
                    section["Avg"], section["Professor"], section["Title"], section["Pass"],
                    section["Fail"], section["Audit"], section["id"].toString(), section["Year"]));
            }
            return course;
        }
    }
}
