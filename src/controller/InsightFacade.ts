import Log from "../Util";
import {IInsightFacade, InsightDataset, InsightDatasetKind, InsightError, NotFoundError} from "./IInsightFacade";
import {ParseZip} from "./ParseZip";
import {Course} from "./Course";
import {InsightQuery, InsightFilter, InsightOptions} from "./Query";
import {isNumber, isString, log} from "util";

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
                        fulfill(result);
                    }).catch(function () {
                        reject(new InsightError());
                    });
            }).catch(function () {
                reject(new InsightError());
            });
        });
    }

    private storeData(id: string, courses: Course[]): Promise<string[]> {
        let t = this;
        let jsonName = id + ".json";
        let jsonToString = JSON.stringify(courses);
        return new Promise(function (fulfill, reject) {
            fs.writeFile(jsonName, jsonToString, function (err: any) {
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
        let jsonName = id + ".json";
        return new Promise(function (fulfill, reject) {
            if (!fs.existsSync(jsonName) || !t.storage.has(id)) {
                reject(new NotFoundError());
            } else {
                t.storage.delete(id);
                fs.unlinkSync(jsonName);
                t.idArray.splice(t.idArray.indexOf(id), 1);
                fulfill(id);
            }
        });
    }
    private filterDataset(dataset: any[], filter: InsightFilter, id: string): any[] {
        const filteredDataset: any[] = [];
        if (Object.keys(filter).length === 0) {
            for (const data of dataset) {
                for (const sec of data.sections) {
                    filteredDataset.push(sec);
                }
            }
        } else {
            for (const data of dataset) {
                for (const sec of data.sections) {
                    if (this.isFilterSatisfied(filter, sec, id)) {
                        filteredDataset.push(sec);
                    }
                }
            }
        }
        if (!filteredDataset) {
            throw  new Error("No satisfied data found.");
        } else {
            return filteredDataset;
        }
    }
    private isFilterSatisfied(filter: InsightFilter, data: any, cid: string): boolean {
        if (Object.keys(filter).length > 1 ) {
            throw new Error("Query is malformed");
        } else if (filter.NOT) {
            return (!(this.isFilterSatisfied(filter.NOT, data, cid)));
        } else if (filter.AND) {
            if (filter.AND.length < 1) {
                throw new Error("at least one filter in AND");
            }
            for (const r of filter.AND) {
                if (Object.keys(r).length < 1) {
                    throw new Error("empty filter");
                }
                if (this.isFilterSatisfied(r, data, cid) === false) {
                    return false;
                }
            }
            return true;
        } else if (filter.OR) {
            if (filter.OR.length < 1) {
                throw new Error("at least one filter in OR");
            }
            for (const r of filter.OR) {
                if (Object.keys(r).length < 1) {
                    throw new Error("empty filter");
                }
                if (this.isFilterSatisfied(r, data, cid) === true) {
                    return true;
                }
            }
            return false;
        } else if (filter.GT) {
            const key = Object.keys(filter.GT)[0];
            const val = filter.GT[key];
            const col = key.split("_")[1];
            const fid = key.split("_")[0];
            const actualValue = data[col];
            if (fid !== cid) {
                throw new Error("Query is trying to query tow datasets a the same time");
            }
            if (!isNumber(val) || !isNumber(actualValue)) {
                throw new Error("GT should be a number");
            } else {
                return actualValue > val ;
            }
        } else if (filter.LT) {
            const key = Object.keys(filter.LT)[0];
            const val = filter.LT[key];
            const col = key.split("_")[1];
            const fid = key.split("_")[0];
            const actualValue = data[col];
            if (fid !== cid) {
                throw new Error("Query is trying to query tow datasets a the same time");
            }
            if (!isNumber(val) || !isNumber(actualValue)) {
                throw new Error("LT should be a number");
            } else {
                return actualValue < val ;
            }
        } else if (filter.EQ) {
            const key = Object.keys(filter.EQ)[0];
            const val = filter.EQ[key];
            const col = key.split("_")[1];
            const fid = key.split("_")[0];
            const actualValue = data[col];
            if (fid !== cid) {
                throw new Error("Query is trying to query tow datasets a the same time");
            }
            if (!isNumber(val) || !isNumber(actualValue)) {
                throw new Error("EQ should be a number");
            } else {
                return actualValue === val ;
            }
        } else if (filter.IS) {
            const key = Object.keys(filter.IS)[0];
            const val: string = filter.IS[key];
            const col = key.split("_")[1];
            const fid = key.split("_")[0];
            const actualValue: string = data[col];
            if (fid !== cid) {
                throw new Error("Query is trying to query tow datasets a the same time");
            }
            if (!isString(val) || !isString(actualValue)) {
                throw new Error("IS should be a string");
            } else {
                if (val === "*") {
                    return true;
                }  else if (val[0] === "*" && val[val.length - 1] === "*") {
                    return actualValue.includes(val.substr(1, val.length - 2));
                }  else if (val[0] === "*") {
                    return actualValue.endsWith(val.slice(1));
                }  else if (val[val.length - 1] === "*") {
                    return actualValue.startsWith(val.slice(0, val.length - 1));
                }  else if (val.includes("*")) {
                    throw  new Error("no star in the middle");
                }   else {
                    return (actualValue === val);
                }
            }
        } else {
            throw new Error("Invalid filter name.");
        }
    }
    private trimDatasetByColumns(dataset: any[], columns: string[]): any[] {
        const trimmedDataset: any[] = [];
        for (const data of dataset) {
            const entry: {[key: string]: any } = {};
            for (const ID_KEY of columns) {
                let key;
                if (ID_KEY.includes("_")) {
                    key = ID_KEY.split("_")[1];
                } else {
                    throw new Error("no _ in key");
                }
                if (data.hasOwnProperty(key)) {
                    entry[ID_KEY] = data[key];
                } else {
                    throw new Error("Invalid Key.");
                }
            }
            trimmedDataset.push(entry);
        }
        return trimmedDataset;
    }
    private sortResult(result: any[], order: any, columns: string[]): any[] {
        if (!columns.includes(order)) {
            throw new InsightError("order is not in the column");
        } else {
            result.sort((left, right): any => {
                if (left[order] > right[order]) {
                    return 1;
                } else if (left[order] < right[order]) {
                    return -1;
                }
            });
            return result;
        }
    }
    public performQuery(query: any): Promise <any[]> {
        return new Promise<any[]>((fulfill, reject) => {
            try {
                const filter: InsightFilter = query.WHERE;
                const options: InsightOptions = query.OPTIONS;
                const columns = options.COLUMNS;
                const order = options.ORDER;
                const id: string = columns[0].split("_")[0];
                let dataset;
                let sections;
                if (this.storage.get(id)) {
                    dataset =  this.storage.get(id);
                } else {
                    throw new Error("invalid id");
                }
                sections = this.filterDataset(dataset, filter, id);
                if (sections.length >= 5000) {
                    throw new Error("too many result");
                }
                let result = this.trimDatasetByColumns(sections, columns);
                if (order) {
                    result = this.sortResult(result, order, columns);
                }
                fulfill(result);
            } catch (err) {
                Log.error(err);
                reject(new InsightError ("invalid query catch at perform") );
            }
        });
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
                Log.trace(rows.toString());
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
