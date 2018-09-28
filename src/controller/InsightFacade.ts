import Log from "../Util";
import {IInsightFacade, InsightDataset, InsightDatasetKind, InsightError, NotFoundError} from "./IInsightFacade";
import {ParseZip} from "./ParseZip";
import {Course} from "./Course";

/**
 * This is the main programmatic entry point for the project.
 * Method documentation is in IInsightFacade
 *
 */

let fs = require("fs");

export default class InsightFacade implements IInsightFacade {
    private parser: ParseZip;
    private idArray: string[];
    private storage: Map<string, Course[]>;

    constructor() {
        this.idArray = [];
        this.storage = new Map<string, Course[]>();
    }

    public addDataset(id: string, content: string, kind: InsightDatasetKind): Promise<string[]> {
        let t = this;
        if (!(kind === InsightDatasetKind.Courses)) {
            return Promise.reject(new InsightError("Kind is not Courses"));
        }
        if (id === null || id === undefined || id === "") {
            return Promise.reject(new InsightError("id is invalid"));
        }
        if (content === null || content === undefined || content === "") {
            return Promise.reject(new InsightError("content is invalid"));
        }
        if (t.idArray.includes(id)) {
            return Promise.reject(new InsightError("content already exists"));
        }
        if (fs.existsSync(id + ".json")) {
            // TODO parse from local disk!!!
        }
        this.parser = new ParseZip();
        return new Promise(function (fulfill, reject) {
            t.parser.parseZip(content).then(function (courses: Course[]) {
                    t.storeData(id, courses).then(function (result: string[]) {
                        Log.warn(result[0]);
                        fulfill(result);
                    }).catch(function () {
                        reject(new InsightError());
                    });
            }).catch(function () {
                Log.error("err 2 in addDataSet");
                reject(new InsightError());
            });
        });
    }

    private storeData(id: string, courses: Course[]): Promise<string[]> {
        let t = this;
        let zipName = id + ".json";
        Log.info(zipName);
        let jsonToString = JSON.stringify(courses);
        return new Promise(function (fulfill, reject) {
            fs.writeFile(zipName, jsonToString, function (err: any) {
                if (!err) {
                    t.storage.set(id, courses);
                    t.idArray.push(id);
                    fulfill(t.idArray);
                } else {
                    reject(new InsightError());
                }
            });
        });
    }

    public removeDataset(id: string): Promise<string> {
        if (id === null || id === undefined) {
            return Promise.reject(new InsightError("invalid id for removal"));
        }
        let t = this;
        let zipName = id + ".json";
        return new Promise(function (fulfill, reject) {
            if (!fs.existsSync(zipName) || !t.storage.has(id)) {
                reject(new NotFoundError());
            } else {
                t.storage.delete(id);
                fs.unlinkSync(zipName);
                t.idArray.splice(t.idArray.indexOf(id), 1);
                fulfill(id);
            }
        });
    }

    public performQuery(query: any): Promise <any[]> {
        return Promise.reject("Not implemented.");
    }

    public listDatasets(): Promise<InsightDataset[]> {
        let t = this;
        let result: InsightDataset[] = [];
        return new Promise<InsightDataset[]>(function (fulfill) {
            for (let entry of Array.from(t.storage.entries())) {
                let id = entry[0];
                let courseArray = entry[1];
                let rows = 0;
                for (let c of courseArray) {
                    rows += c.numberOfSections();
                }
                let course: InsightDataset = t.createInsightDataSet(id, InsightDatasetKind.Courses, rows);
                Log.warn(rows.toString());
                result.push(course);
            }
            fulfill(result);
        });
    }

    public createInsightDataSet (id2: string, kind2: InsightDatasetKind, numRows2: number) {
        return {
            id: id2,
            kind: kind2,
            numRows: numRows2
        };
    }
}
