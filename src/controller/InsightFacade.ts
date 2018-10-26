import Log from "../Util";
import {IInsightFacade, InsightDataset, InsightDatasetKind, InsightError, NotFoundError} from "./IInsightFacade";
import {ParseZip} from "./ParseZip";
import {ParseZipRoom} from "./ParseZipRoom";
import {Course} from "./Course";
import {InsightFilter, InsightOptions, InsightTransformation} from "./Query";
import {isNumber, isString} from "util";
import {existsSync} from "fs";
import {Building} from "./Building";
import {Decimal} from "decimal.js";
import {Room} from "./Room";
import {Section} from "./Section";
import _ = require("lodash");
/**
 * This is the main programmatic entry point for the project.
 * Method documentation is in IInsightFacade
 *
 */
let fs = require("fs");
let directory =  __dirname + "/../../data";

export default class InsightFacade implements IInsightFacade {
    private parser: any;
    private idArray: string[];
    private storage: Map<string, any[]>;

    constructor() {
        this.idArray = [];
        this.storage = new Map<string, any[]>();
        if (!existsSync(directory)) {
            fs.mkdirSync(directory);
        }
    }

    public addDataset(id: string, content: string, kind: InsightDatasetKind): Promise<string[]> {
        if (!(kind === InsightDatasetKind.Courses || kind === InsightDatasetKind.Rooms)) {
            return Promise.reject(new InsightError("Kind is invalid"));
        }
        if (id === null || id === undefined || id === "") {
            return Promise.reject(new InsightError("id is invalid"));
        }
        if (content === null || content === undefined || content === "") {
            return Promise.reject(new InsightError("content is invalid"));
        }
        if (this.idArray.includes(id)) {
            return Promise.reject(new InsightError("content already exists"));
        }
        if (kind === InsightDatasetKind.Courses) {
            return this.parseCourse(id, content);
        } else {
            return this.parseRoom(id, content);
        }
    }

    private parseCourse(id: string, content: string): Promise<string[]> {
        let t = this;
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

    private parseRoom(id: string, content: string): Promise<string[]> {
        let t = this;
        this.parser = new ParseZipRoom();
        return new Promise(function (fulfill, reject) {
            t.parser.parseZipRoom(content).then(function (buildings: Building[]) {
                t.storeData(id, buildings).then(function (result: string[]) {
                    fulfill(result);
                }).catch(function () {
                    reject(new InsightError());
                });
            }).catch(function () {
                reject(new InsightError());
            });
        });
    }

    private storeData(id: string, data: any[]): Promise<string[]> {
        let t = this;
        let path = directory + "/" + id + ".json";
        let jsonToString = JSON.stringify(data);
        return new Promise(function (fulfill, reject) {
            fs.writeFile(path, jsonToString, function (err: any) {
                if (!err) {
                    t.storage.set(id, data);
                    t.idArray.push(id);
                    Log.warn("fulfill");
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
        let path = directory + "/" + id + ".json";
        return new Promise(function (fulfill, reject) {
            if (!fs.existsSync(path) || !t.storage.has(id)) {
                reject(new NotFoundError());
            } else {
                t.storage.delete(id);
                fs.unlinkSync(path);
                t.idArray.splice(t.idArray.indexOf(id), 1);
                fulfill(id);
            }
        });
    }
    private produceFilteredSections(courseSet: any[], filter: InsightFilter, id: string): any[] {
        const filteredDataset: any[] = [];
        if (courseSet[0] instanceof Building) {
            if (Object.keys(filter).length === 0) {
                for (const data of courseSet) {
                    for (const sec of data.rooms) {
                        filteredDataset.push(sec);
                    }
                }
            } else {
                for (const data of courseSet) {
                    for (const sec of data.rooms) {
                        if (this.isSatisfied(filter, sec, id)) {
                            filteredDataset.push(sec);
                        }
                    }
                }
            }
        } else {
            if (Object.keys(filter).length === 0) {
                for (const data of courseSet) {
                    for (const sec of data.sections) {
                        filteredDataset.push(sec);
                    }
                }
            } else {
                for (const data of courseSet) {
                    for (const sec of data.sections) {
                        if (this.isSatisfied(filter, sec, id)) {
                            filteredDataset.push(sec);
                        }
                    }
                }
            }
        }
        return filteredDataset;

    }
    private isSatisfied(filter: InsightFilter, data: any, datasetID: string): boolean {
        if (Object.keys(filter).length > 1 ) {
            throw new Error("Query is malformed");
        } else if (filter.NOT) {
            if (Object.keys(filter.NOT).length > 1) {
                throw new Error("more than one filter in NOT");
            }
            return (!(this.isSatisfied(filter.NOT, data, datasetID)));
        } else if (filter.AND) {
            if (filter.AND.length < 1) {
                throw new Error("at least one filter in AND");
            }
            for (const r of filter.AND) {
                if (Object.keys(r).length < 1) {
                    throw new Error("empty filter");
                }
                if (this.isSatisfied(r, data, datasetID) === false) {
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
                if (this.isSatisfied(r, data, datasetID) === true) {
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
            if (fid !== datasetID) {
                throw new Error("Query is trying to query two datasets a the same time");
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
            if (fid !== datasetID) {
                throw new Error("Query is trying to query two datasets a the same time");
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
            if (fid !== datasetID) {
                throw new Error("Query is trying to query two datasets a the same time");
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
            if (fid !== datasetID) {
                throw new Error("Query is trying to query two datasets a the same time");
            }
            if (!isString(val) || !isString(actualValue)) {
                throw new Error("IS should be a string");
            } else {
                if (val === "*") {
                    return true;
                }   else if (val[0] === "*" && val[val.length - 1] !== "*") {
                    return actualValue.endsWith(val.slice(1));
                }   else if (val[val.length - 1] === "*" && val[0] !== "*") {
                    return actualValue.startsWith(val.slice(0, val.length - 1));
                }   else if (val[0] === "*" && val[val.length - 1] === "*") {
                    return actualValue.includes(val.substr(1, val.length - 2));
                }   else if (val.includes("*")) {
                    throw  new Error("no star in the middle");
                }   else {
                    return (actualValue === val);
                }
            }
        } else {
            throw new Error("Invalid filter name.");
        }
    }
    private transform(dataset: any[], id: string, transformation: InsightTransformation): any [] {
        const transformedDataset = [];
        if (Object.keys(transformation.GROUP).length < 1) {
            throw new Error("at least one key in Group");
        }
        let dataDict: {[key: string]: any} = {};
        for (const data of dataset) {
            let a: string = "";
            for (const i of transformation.GROUP) {
                if (! i.includes("_")) {
                    throw new Error("no _ in group key");
                }
                const gid = i.split("_")[0];
                if (gid !== id) {
                    throw new Error("wrong dataset id in group key");
                }
                const key = i.split("_")[1];
                if (data[key] === undefined) {
                    throw new Error("invalid key in group key");
                }
                a = a + (data[key] as string);
            }
            if (dataDict[a] === undefined) {
                dataDict[a] = [data];
            } else {
                dataDict[a].push(data);
            }
        }
        const rules = transformation.APPLY;
        if (Object.keys(rules).length < 1) {
            throw new Error("at least one rule in apply");
        }
        for (let k in dataDict) {
            const sections = dataDict [k];
            const group: {[key: string]: any} = {};
            for (const g of transformation.GROUP) {
                const key = g.split("_")[1];
                group[key] = sections[0][key];
            }
            for ( const rule of rules) {
                const applyKey = Object.keys(rule)[0];
                if (applyKey.length < 1) {
                    throw new Error("at least one character in applykey");
                }
                if (applyKey.includes("_")) {
                    throw new Error("there should not be _ in applykey");
                }
                if (group[applyKey]) {
                    throw new Error("duplicate applykeys in two rules");
                }
                const token = Object.keys(rule[applyKey])[0];
                const ID_KEY = rule[applyKey][token];
                if (! ID_KEY.includes("_")) {
                    throw new Error("no _ in key in token");
                }
                if (ID_KEY.split("_")[0] !== id) {
                    throw new Error("wrong dataset id in token key");
                }
                const key = ID_KEY.split("_")[1];
                const toCompute: any[] = [];
                for (const section of sections) {
                    toCompute.push(section[key]);
                }
                let value: number;
                if (token === "MAX") {
                    if (isNumber(toCompute[0])) {
                        const numToCompute = toCompute as number[];
                        value = Math.max.apply(null, numToCompute);
                    } else {
                        throw new Error("not a number in max");
                    }
                } else if (token === "MIN") {
                    if (isNumber(toCompute[0])) {
                        const numToCompute = toCompute as number[];
                        value = Math.min.apply(null, numToCompute);
                    } else {
                        throw new Error("not a number in min");
                    }
                } else if (token === "AVG") {
                    if (isNumber(toCompute[0])) {
                        const numToCompute = toCompute as number[];
                        let sum = new Decimal(0);
                        for (const e of numToCompute) {
                            sum.add(new Decimal(e));
                        }
                        const avg = sum.toNumber() / numToCompute.length;
                        value = Number(avg.toFixed(2));
                    } else {
                        throw new Error("not a number in avg");
                    }
                } else if (token === "COUNT") {
                    value = new Set(toCompute).size;
                } else if (token === "SUM") {
                    if (isNumber(toCompute[0])) {
                        const numToCompute = toCompute as number[];
                        let sum = new Decimal(0);
                        for (const e of numToCompute) {
                            sum.add(new Decimal(e));
                        }
                        value = Number(sum.toFixed(2));
                    } else {
                        throw new Error("not a number in sum");
                    }
                } else {
                    throw new Error("invalid token");
                }
                group[applyKey] = value;
            }
            transformedDataset.push(group);
        }
        return transformedDataset;
    }
    public performQuery(query: any): Promise <any[]> {
        return new Promise<any[]>((fulfill, reject) => {
            try {
                const filter: InsightFilter = query.WHERE;
                const options: InsightOptions = query.OPTIONS;
                if (Object.keys(options).length === 0)  {
                    throw new Error("empty option");
                }
                const columns = options.COLUMNS;
                if (Object.keys(columns).length === 0) {
                    throw new Error("empty column");
                }
                const transformation: InsightTransformation = query.TRANSFORMATIONS;
                const order = options.ORDER;
                if (!columns[0].includes("_")) {
                    throw new Error("no _ in column");
                }
                const id: string = columns[0].split("_")[0];
                let courseDataset;
                let sections;
                if (this.storage.get(id)) {
                    courseDataset =  this.storage.get(id);
                } else {
                    throw new Error("invalid id");
                }
                sections = this.produceFilteredSections(courseDataset, filter, id);
                if (sections.length > 5000) {
                    throw new Error("too many result");
                }
                let groups: any[] = [];
                if (transformation) {
                    if (Object.keys(transformation).length !== 2) {
                        throw new Error("invalid transofrmation");
                    }
                    groups = this.transform(sections, id, transformation);
                }
                let result: any[] = [];
                if (transformation) {
                    for (const group of groups) {
                        const newGroup: {[key: string]: any } = {};
                        for (const ID_KEY of columns) {
                            if (this.showsUpinTransform(ID_KEY, transformation) ) {
                                let key;
                                if (! ID_KEY.includes("_")) {
                                    key = ID_KEY;
                                } else {
                                    key = ID_KEY.split("_")[1];
                                }
                                if ( ! group.hasOwnProperty(key)) {
                                    throw new Error("Invalid Key.");
                                }
                                newGroup[ID_KEY] = group[key];
                            } else {
                                throw new Error("colomn is not in group or applykey");
                            }
                        }
                        result.push(newGroup);
                    }
                } else {
                    for (const sec of sections) {
                        const newGroup: {[key: string]: any } = {};
                        for (const ID_KEY of columns) {
                            if (! ID_KEY.includes("_")) {
                                throw new Error("no _ in key");
                            }
                            const key = ID_KEY.split("_")[1];
                            if ( ! sec.hasOwnProperty(key)) {
                                throw new Error("Invalid Key.");
                            }
                            newGroup[ID_KEY] = sec[key];
                        }
                        result.push(newGroup);
                    }
                }
                if (order) {
                    if (isString(order)) {
                        if (!columns.includes(order)) {
                            throw new InsightError("order is not in the column");
                        }
                        result.sort((a, b) => {
                            if (a[order] < b[order]) {
                                return -1;
                            }
                            if (a[order] > b[order]) {
                                return 1;
                            }
                            return 0;
                        });
                    } else {
                        const applykeys = order.keys;
                        if (applykeys.length < 1) {
                            throw new Error("at least 1 key in order key");
                        }
                        const dir = order.dir;
                        if (dir.length === 0) {
                            throw new Error("empty error");
                        }
                        function sortHelper1(a: any, b: any , keys: string[], index: number): number {
                            if (a[keys[index]] < b[keys[index]]) {
                                return -1;
                            }
                            if (a[keys[index]] > b[keys[index]]) {
                                return 1;
                            }
                            if (index >= applykeys.length - 1 ) {
                                return 0;
                            }
                            return sortHelper1(a, b, keys, index + 1);
                        }
                        function sortHelper2(a: any, b: any , keys: string[], index: number): number {
                            if (a[keys[index]] < b[keys[index]]) {
                                return 1;
                            }
                            if (a[keys[index]] > b[keys[index]]) {
                                return -1;
                            }
                            if (index >= applykeys.length - 1 ) {
                                return 0;
                            }
                            return sortHelper2(a, b, keys, index + 1);
                        }
                        result.sort((a, b) => {
                            if (dir === "UP") {
                                return sortHelper1(a, b, applykeys, 0);
                            }
                            if (dir === "DOWN") {
                                return sortHelper2(a, b, applykeys, 0);
                            }
                            throw  new Error("invalid dir");
                        });
                    }

                }
                fulfill(result);
            } catch (err) {
                Log.error(err);
                reject(new InsightError ("invalid query catch at perform") );
            }
        });
    }
    private  showsUpinTransform(ID_KEY: string, transformation: InsightTransformation): boolean {
        if (transformation.GROUP.includes(ID_KEY)) {
            return true;
        }
        for (const rule of transformation.APPLY) {
            if (Object.keys(rule).includes(ID_KEY)) {
                return true;
            }
        }
        return false;
    }
    public listDatasets(): Promise<InsightDataset[]> {
        let t = this;
        let result: InsightDataset[] = [];
        return new Promise<InsightDataset[]>(function (fulfill) {
            for (let entry of Array.from(t.storage.entries())) {
                let id = entry[0];
                let value = entry[1];
                let rows = 0;
                if (value[0].getType() === InsightDatasetKind.Courses) {
                    for (let c of value) {
                        rows += c.numberOfSections();
                    }
                    let course: InsightDataset = t.createInsightDataSet(id, InsightDatasetKind.Courses, rows);
                    Log.trace(rows.toString());
                    result.push(course);
                } else {
                    for (let b of value) {
                        rows += b.numberOfRooms();
                    }
                    let rooms: InsightDataset = t.createInsightDataSet(id, InsightDatasetKind.Rooms, rows);
                    Log.trace(rows.toString());
                    result.push(rooms);
                }
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
