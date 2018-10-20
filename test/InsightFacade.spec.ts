import {expect} from "chai";

import {InsightDataset, InsightDatasetKind, InsightError, NotFoundError} from "../src/controller/IInsightFacade";
import InsightFacade from "../src/controller/InsightFacade";
import Log from "../src/Util";
import TestUtil from "./TestUtil";

// This should match the JSON schema described in test/query.schema.json
// except 'filename' which is injected when the file is read.
export interface ITestQuery {
    title: string;
    query: any;  // make any to allow testing structurally invalid queries
    isQueryValid: boolean;
    result: string | string[];
    filename: string;  // This is injected when reading the file
}

describe("InsightFacade Add/Remove Dataset", function () {
    // Reference any datasets you've added to test/data here and they will
    // automatically be loaded in the Before All hook.
    const datasetsToLoad: { [id: string]: string } = {
        courses: "./test/data/courses.zip",
        test_oneValidOneInvalid: "./test/data/test_oneValidOneInvalid.zip",
        test_oneValidOneInvalid2: "./test/data/test_oneValidOneInvalid2.zip",
        test_oneValidOneInvalid3: "./test/data/test_oneValidOneInvalid3.zip",
        test_oneValidOneInvalid4: "./test/data/test_oneValidOneInvalid4.zip",
        test_oneValidTwoInvalid: "./test/data/test_oneValidTwoInvalid.zip",
        test_oneValidTwoInvalid2: "./test/data/test_oneValidTwoInvalid2.zip",
        test_oneValidTwoInvalid3: "./test/data/test_oneValidTwoInvalid3.zip",
        test_oneValidTwoInvalid4: "./test/data/test_oneValidTwoInvalid4.zip",
        test_allValid: "./test/data/test_allValid.zip",
        test_someValidWithJSONInvalid: "./test/data/test_someValidWithJSONInvalid.zip",
        test_someValidWithNoSection: "./test/data/test_someValidWithNoSection.zip",
        test_someValidWithNoJSON: "./test/data/test_someValidWithNoJSON.zip",
        test_someValidWithBoth: "./test/data/test_someValidWithBoth.zip",
        test_courses: "./test/data/test_courses.zip",
        test_oneValid: "./test/data/test_oneValid.zip",
        test_otherFolderInside: "./test/data/test_otherFolderInside.zip",
        test_missingElementButValid: "./test/data/test_missingElementButValid.zip",
        test_samePath: "./test/data/courses.zip",
        test_allInvalid: "./test/data/test_allInvalid.zip",
        test_allInvalid2: "./test/data/test_allInvalid2.zip",
        test_zeroValid1: "./test/data/test_zeroValid1.zip",
        test_zeroValid2: "./test/data/test_zeroValid2.zip",
        test_invalidJSON1: "./test/data/test_invalidJSON1.zip",
        test_textFile: "./test/data/test_textFile.txt",
        test_infiniteZip: "./test/data/test_infiniteZip.zip",
        test_noFolderInside: "./test/data/test_noFolderInside.zip",
        test_noCorrectFolder: "./test/data/test_noCorrectFolder.zip",
        test_notNamedCourses: "./test/data/test_notNamedCourses.zip",
        test_manyFoldersInvalid: "./test/data/test_manyFoldersInvalid.zip",
        test_manyFoldersNoCourses: "./test/data/test_manyFoldersNoCourses.zip",
        test_manyFoldersValid: "./test/data/test_manyFoldersValid.zip",
        test_manyFolders: "./test/data/test_manyFolders.zip",
        test_coursesJustOneSection: "./test/data/test_coursesJustOneSection.zip",
        test_empty: "./test/data/test_empty.zip",
        test_emptyJSON: "./test/data/test_emptyJSON.zip",
        test_nonJSONInside: "./test/data/test_nonJSONInside.zip",
        test_noJSONFile: "./test/data/test_noJSONFile.zip",
        rooms: "./test/data/rooms.zip",
    };

    let insightFacade: InsightFacade;
    let datasets: { [id: string]: string };

    before(async function () {
        Log.test(`Before: ${this.test.parent.title}`);

        try {
            const loadDatasetPromises: Array<Promise<Buffer>> = [];
            for (const [id, path] of Object.entries(datasetsToLoad)) {
                loadDatasetPromises.push(TestUtil.readFileAsync(path));
            }
            const loadedDatasets = (await Promise.all(loadDatasetPromises)).map((buf, i) => {
                return { [Object.keys(datasetsToLoad)[i]]: buf.toString("base64") };
            });
            datasets = Object.assign({}, ...loadedDatasets);
            expect(Object.keys(datasets)).to.have.length.greaterThan(0);
        } catch (err) {
            expect.fail("", "", `Failed to read one or more datasets. ${JSON.stringify(err)}`);
        }

        try {
            insightFacade = new InsightFacade();
        } catch (err) {
            Log.error(err);
        } finally {
            expect(insightFacade).to.be.instanceOf(InsightFacade);
        }
    });

    beforeEach(function () {
        Log.test(`BeforeTest: ${this.currentTest.title}`);
    });

    after(function () {
        Log.test(`After: ${this.test.parent.title}`);
    });

    afterEach(function () {
        Log.test(`AfterTest: ${this.currentTest.title}`);
    });

    it("Should add a valid dataset", async () => {
        const id: string = "courses";
        let response: string[];

        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.deep.equal([id]);
        }
    });

    it("Should return the list of dataset 1", async () => {
        const expected: InsightDataset = {id: "courses", kind: InsightDatasetKind.Courses, numRows: 64612};
        let response: any[];
        try {
            response = await insightFacade.listDatasets();
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.deep.equal
            ([expected]);
        }
    });

    it("should be added: 1 valid course and 1 invalid course with no course section", async () => {
        const id: string = "test_oneValidOneInvalid";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.deep.equal(["courses", id]);
        }
    });

    it("should be added: 1 valid course and 1 invalid course with wrong JSON format", async () => {
        const id: string = "test_oneValidOneInvalid2";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.deep.equal(["courses", "test_oneValidOneInvalid", id]);
        }
    });

    it("should be added: 1 valid course and 1 invalid course with wrong JSON format as well", async () => {
        const id: string = "test_oneValidOneInvalid3";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.deep.equal(["courses", "test_oneValidOneInvalid", "test_oneValidOneInvalid2", id]);
        }
    });

    it("should be added: 1 valid course and 1 invalid course with non JSON file", async () => {
        const id: string = "test_oneValidOneInvalid4";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.deep.equal
            (["courses", "test_oneValidOneInvalid", "test_oneValidOneInvalid2", "test_oneValidOneInvalid3", id]);
        }
    });

    // This is an example of a pending test. Add a callback function to make the test run.
    it("Should remove the courses dataset courses", async () => {
        const id: string = "courses";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.equal(id);
        }
    });

    // This is an example of a pending test. Add a callback function to make the test run.
    it("Should not remove the courses dataset courses again", async () => {
        const id: string = "courses";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(NotFoundError);
        }
    });

    it("Should not remove the courses not found", async () => {
        const id: string = "DNE";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(NotFoundError);
        }
    });

    it("Should not remove the courses not found empty str", async () => {
        const id: string = "";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(NotFoundError);
        }
    });

    it("Should not remove the courses not found space str", async () => {
        const id: string = " ";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(NotFoundError);
        }
    });

    it("Should not remove the courses not found null", async () => {
        const id: string = null;
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("Should not remove the courses not found undefined", async () => {
        const id: string = undefined;
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("Should remove the courses dataset test_oneValidOneInvalid", async () => {
        const id: string = "test_oneValidOneInvalid";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.equal(id);
        }
    });

    it("Should remove the courses dataset test_oneValidOneInvalid2", async () => {
        const id: string = "test_oneValidOneInvalid2";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.equal(id);
        }
    });

    it("Should remove the courses dataset test_oneValidOneInvalid3", async () => {
        const id: string = "test_oneValidOneInvalid3";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.equal(id);
        }
    });

    it("Should remove the courses dataset test_oneValidOneInvalid4", async () => {
        const id: string = "test_oneValidOneInvalid4";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.equal(id);
        }
    });

    it("Should return the list of dataset 2", async () => {
        let response: any[];
        try {
            response = await insightFacade.listDatasets();
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.deep.equal([]);
        }
    });

    it("Should not remove the courses dataset test_oneValidOneInvalid when empty", async () => {
        const id: string = "test_oneValidOneInvalid";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(NotFoundError);
        }
    });

    it("should be added: 1 valid course, 1 invalid JSON course, and 1 empty course", async () => {
        const id: string = "test_oneValidTwoInvalid";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.deep.equal([id]);
        }
    });

    it("should be added: 1 valid course, 1 invalid JSON course, and 1 invalid section course", async () => {
        const id: string = "test_oneValidTwoInvalid2";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.deep.equal(["test_oneValidTwoInvalid", id]);
        }
    });

    it("should be added: 1 valid course, 1 invalid JSON course, and 1 non-JSON file", async () => {
        const id: string = "test_oneValidTwoInvalid3";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.deep.equal(["test_oneValidTwoInvalid", "test_oneValidTwoInvalid2", id]);
        }
    });

    it("should be added: 1 valid course, 1 empty section course, and 1 non-JSON file", async () => {
        const id: string = "test_oneValidTwoInvalid4";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.deep.equal
            (["test_oneValidTwoInvalid", "test_oneValidTwoInvalid2", "test_oneValidTwoInvalid3", id]);
        }
    });

    it("Should remove the courses dataset test_oneValidTwoInvalid4", async () => {
        const id: string = "test_oneValidTwoInvalid4";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.equal(id);
        }
    });

    it("Should remove the courses dataset test_oneValidTwoInvalid3", async () => {
        const id: string = "test_oneValidTwoInvalid3";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.equal(id);
        }
    });

    it("Should remove the courses dataset test_oneValidTwoInvalid2", async () => {
        const id: string = "test_oneValidTwoInvalid2";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.equal(id);
        }
    });

    it("Should remove the courses dataset test_oneValidTwoInvalid", async () => {
        const id: string = "test_oneValidTwoInvalid";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.equal(id);
        }
    });

    it("should be added: 3 valid courses", async () => {
        const id: string = "test_allValid";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.deep.equal([id]);
        }
    });

    it("Should remove the courses dataset all_valid", async () => {
        const id: string = "test_allValid";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.deep.equal(id);
        }
    });

    it("Should return the list of dataset 3", async () => {
        let response: any[];
        try {
            response = await insightFacade.listDatasets();
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.deep.equal([]);
        }
    });

    it("should be added: some valid with JSON invalid", async () => {
        const id: string = "test_someValidWithJSONInvalid";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.deep.equal([id]);
        }
    });

    it("should be added: some valid with no section", async () => {
        const id: string = "test_someValidWithNoSection";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.deep.equal(["test_someValidWithJSONInvalid", id]);
        }
    });

    it("should be added: some valid with no JSON", async () => {
        const id: string = "test_someValidWithNoJSON";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.deep.equal(["test_someValidWithJSONInvalid", "test_someValidWithNoSection", id]);
        }
    });

    it("should be added: some valid with JSON invalid and no section invalid", async () => {
        const id: string = "test_someValidWithBoth";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.deep.equal
            (["test_someValidWithJSONInvalid", "test_someValidWithNoSection", "test_someValidWithNoJSON", id]);
        }
    });

    it("Should remove the courses dataset test_someValidWithJSONInvalid", async () => {
        const id: string = "test_someValidWithJSONInvalid";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.deep.equal(id);
        }
    });

    it("Should remove the courses dataset test_someValidWithNoSection", async () => {
        const id: string = "test_someValidWithNoSection";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.deep.equal(id);
        }
    });

    it("Should remove the courses dataset test_someValidWithNoJSON", async () => {
        const id: string = "test_someValidWithNoJSON";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.deep.equal(id);
        }
    });

    it("Should remove the courses dataset test_someValidWithBoth", async () => {
        const id: string = "test_someValidWithBoth";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.deep.equal(id);
        }
    });

    it("should be added : test_courses", async () => {
        const id: string = "test_courses";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.deep.equal([id]);
        }
    });

    it("Should remove the courses dataset test_courses", async () => {
        const id: string = "test_courses";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.equal(id);
        }
    });

    it("should be added: 1 valid course", async () => {
        const id: string = "test_oneValid";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.deep.equal([id]);
        }
    });

    it("Should remove the courses dataset test_oneValid", async () => {
        const id: string = "test_oneValid";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.equal(id);
        }
    });

    it("should be added even if other folders inside", async () => {
        const id: string = "test_otherFolderInside";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.deep.equal([id]);
        }
    });

    it("Should remove the courses dataset test_otherFolderInside", async () => {
        const id: string = "test_otherFolderInside";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.equal(id);
        }
    });

    it("should be added even if missing one element", async () => {
        const id: string = "test_missingElementButValid";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.deep.equal([id]);
        }
    });

    it("Should remove the courses dataset test_missingElementButValid", async () => {
        const id: string = "test_missingElementButValid";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.equal(id);
        }
    });

    it("should be added even if it has same path", async () => {
        const id: string = "test_samePath";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.deep.equal([id]);
        }
    });

    it("Should remove the courses dataset test_samePath", async () => {
        const id: string = "test_samePath";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.equal(id);
        }
    });

    it("should be added even if many folders valid", async () => {
        const id: string = "test_manyFoldersValid";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.deep.equal([id]);
        }
    });

    it("Should remove the courses dataset test_manyFoldersValid", async () => {
        const id: string = "test_manyFoldersValid";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.equal(id);
        }
    });

    it("should be added even if many folders", async () => {
        const id: string = "test_manyFolders";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.deep.equal([id]);
        }
    });

    it("Should remove the courses dataset test_manyFolders", async () => {
        const id: string = "test_manyFolders";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.equal(id);
        }
    });

    it("should be added test_coursesJustOneSection", async () => {
        const id: string = "test_coursesJustOneSection";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.deep.equal([id]);
        }
    });

    it("Should remove the courses dataset test_coursesJustOneSection", async () => {
        const id: string = "test_coursesJustOneSection";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.equal(id);
        }
    });

    it("should not be added: 3 invalid courses (2 JSON 1 no section)", async () => {
        const id: string = "test_allInvalid";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added: 3 invalid JSON courses", async () => {
        const id: string = "test_allInvalid2";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("Should add a valid dataset", async () => {
        const id: string = "courses";
        let response: string[];

        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.deep.equal([id]);
        }
    });

    it("Should not add a valid repeated dataset", async () => {
        const id: string = "courses";
        let response: string[];

        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to 0 valid course sections 1", async () => {
        const id: string = "test_zeroValid1";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to 0 valid course sections 4", async () => {
        const id: string = "test_zeroValid2";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to invalid JSON format", async () => {
        const id: string = "test_invalidJSON1";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to non-zip format", async () => {
        const id: string = "test_textFile";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to infinite zip", async () => {
        const id: string = "test_infiniteZip";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to no folder inside", async () => {
        const id: string = "test_noFolderInside";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to no correct folder inside", async () => {
        const id: string = "test_noCorrectFolder";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to not named courses", async () => {
        const id: string = "test_notNamedCourses";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due test_manyFoldersInvalid", async () => {
        const id: string = "test_manyFoldersInvalid";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due test_manyFoldersNoCourses", async () => {
        const id: string = "test_manyFoldersNoCourses";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to empty file", async () => {
        const id: string = "test_empty";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to empty JSON file", async () => {
        const id: string = "test_emptyJSON";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to non JSON file inside", async () => {
        const id: string = "test_nonJSONInside";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to incorrect input str str", async () => {
        let response: string[];
        try {
            response = await insightFacade.addDataset("", "", InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to incorrect input null str", async () => {
        let response: string[];
        try {
            response = await insightFacade.addDataset(null, "", InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to incorrect input str null", async () => {
        let response: string[];
        try {
            response = await insightFacade.addDataset("", null, InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to incorrect input null null", async () => {
        let response: string[];
        try {
            response = await insightFacade.addDataset(null, null, InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to incorrect input undefined undefined", async () => {
        let response: string[];
        try {
            response = await insightFacade.addDataset(undefined, undefined, InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to incorrect input undefined str", async () => {
        let response: string[];
        try {
            response = await insightFacade.addDataset(undefined, "", InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to incorrect input str undefined", async () => {
        let response: string[];
        try {
            response = await insightFacade.addDataset("", undefined, InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to incorrect input null undefined", async () => {
        let response: string[];
        try {
            response = await insightFacade.addDataset(null, undefined, InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to incorrect input undefined null", async () => {
        let response: string[];
        try {
            response = await insightFacade.addDataset(undefined, null, InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to incorrect input str str rooms", async () => {
        let response: string[];
        try {
            response = await insightFacade.addDataset("", "", InsightDatasetKind.Rooms);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to incorrect input null str rooms", async () => {
        let response: string[];
        try {
            response = await insightFacade.addDataset(null, "", InsightDatasetKind.Rooms);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to incorrect input str null rooms", async () => {
        let response: string[];
        try {
            response = await insightFacade.addDataset("", null, InsightDatasetKind.Rooms);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to incorrect input null null rooms", async () => {
        let response: string[];
        try {
            response = await insightFacade.addDataset(null, null, InsightDatasetKind.Rooms);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to incorrect input undefined undefined rooms", async () => {
        let response: string[];
        try {
            response = await insightFacade.addDataset(undefined, undefined, InsightDatasetKind.Rooms);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to incorrect input undefined str rooms", async () => {
        let response: string[];
        try {
            response = await insightFacade.addDataset(undefined, "", InsightDatasetKind.Rooms);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to incorrect input str undefined rooms", async () => {
        let response: string[];
        try {
            response = await insightFacade.addDataset("", undefined, InsightDatasetKind.Rooms);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to incorrect input null undefined rooms", async () => {
        let response: string[];
        try {
            response = await insightFacade.addDataset(null, undefined, InsightDatasetKind.Rooms);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to incorrect input undefined null rooms", async () => {
        let response: string[];
        try {
            response = await insightFacade.addDataset(undefined, null, InsightDatasetKind.Rooms);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to incorrect input str str null", async () => {
        let response: string[];
        try {
            response = await insightFacade.addDataset("", "", null);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to incorrect input null str null", async () => {
        let response: string[];
        try {
            response = await insightFacade.addDataset(null, "", null);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to incorrect input str null null", async () => {
        let response: string[];
        try {
            response = await insightFacade.addDataset("", null, null);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to incorrect input null null null", async () => {
        let response: string[];
        try {
            response = await insightFacade.addDataset(null, null, null);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to incorrect input undefined undefined null", async () => {
        let response: string[];
        try {
            response = await insightFacade.addDataset(undefined, undefined, null);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to incorrect input undefined str null", async () => {
        let response: string[];
        try {
            response = await insightFacade.addDataset(undefined, "", null);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to incorrect input str undefined null", async () => {
        let response: string[];
        try {
            response = await insightFacade.addDataset("", undefined, null);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to incorrect input null undefined null", async () => {
        let response: string[];
        try {
            response = await insightFacade.addDataset(null, undefined, null);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to incorrect input undefined null null", async () => {
        let response: string[];
        try {
            response = await insightFacade.addDataset(undefined, null, null);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to incorrect input str str undefined", async () => {
        let response: string[];
        try {
            response = await insightFacade.addDataset("", "", undefined);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to incorrect input null str undefined", async () => {
        let response: string[];
        try {
            response = await insightFacade.addDataset(null, "", undefined);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to incorrect input str null undefined", async () => {
        let response: string[];
        try {
            response = await insightFacade.addDataset("", null, undefined);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to incorrect input null null undefined", async () => {
        let response: string[];
        try {
            response = await insightFacade.addDataset(null, null, undefined);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to incorrect input undefined undefined undefined", async () => {
        let response: string[];
        try {
            response = await insightFacade.addDataset(undefined, undefined, undefined);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to incorrect input undefined str undefined", async () => {
        let response: string[];
        try {
            response = await insightFacade.addDataset(undefined, "", undefined);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to incorrect input str undefined undefined", async () => {
        let response: string[];
        try {
            response = await insightFacade.addDataset("", undefined, undefined);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to incorrect input null undefined undefined", async () => {
        let response: string[];
        try {
            response = await insightFacade.addDataset(null, undefined, undefined);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added due to incorrect input undefined null undefined", async () => {
        let response: string[];
        try {
            response = await insightFacade.addDataset(undefined, null, undefined);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("should not be added: no JSON file", async () => {
        const id: string = "test_noJSONFile";
        let response: string[];
        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Courses);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.be.instanceOf(InsightError);
        }
    });

    it("Should add a valid room dataset", async () => {
        const id: string = "rooms";
        let response: string[];

        try {
            response = await insightFacade.addDataset(id, datasets[id], InsightDatasetKind.Rooms);
        } catch (err) {
            response = err;
        } finally {
            // Log.info(JSON.stringify(response));
            expect(response).to.deep.equal(["courses", id]);
        }
    });

    it("Should return the list of dataset 4", async () => {
        const expected1: InsightDataset = {id: "courses", kind: InsightDatasetKind.Courses, numRows: 64612};
        const expected2: InsightDataset = {id: "rooms", kind: InsightDatasetKind.Rooms, numRows: 364};
        let response: any[];
        try {
            response = await insightFacade.listDatasets();
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.deep.equal
            ([expected1, expected2]);
        }
    });

    it("Should remove the courses dataset", async () => {
        const id: string = "courses";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.equal(id);
        }
    });

    it("Should return the list of dataset 5", async () => {
        const expected2: InsightDataset = {id: "rooms", kind: InsightDatasetKind.Rooms, numRows: 364};
        let response: any[];
        try {
            response = await insightFacade.listDatasets();
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.deep.equal
            ([expected2]);
        }
    });

    it("Should remove the rooms dataset", async () => {
        const id: string = "rooms";
        let response: string;
        try {
            response = await insightFacade.removeDataset(id);
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.equal(id);
        }
    });

    it("Should return the list of dataset 6", async () => {
        let response: any[];
        try {
            response = await insightFacade.listDatasets();
        } catch (err) {
            response = err;
        } finally {
            expect(response).to.deep.equal
            ([]);
        }
    });
});

// This test suite dynamically generates tests from the JSON files in test/queries.
// You should not need to modify it; instead, add additional files to the queries directory.
describe("InsightFacade PerformQuery", () => {
    const datasetsToQuery: { [id: string]: string } = {
        courses: "./test/data/courses.zip",
    };
    let insightFacade: InsightFacade;
    let testQueries: ITestQuery[] = [];

    // Create a new instance of InsightFacade, read in the test queries from test/queries and
    // add the datasets specified in datasetsToQuery.
    before(async function () {
        Log.test(`Before: ${this.test.parent.title}`);

        // Load the query JSON files under test/queries.
        // Fail if there is a problem reading ANY query.
        try {
            testQueries = await TestUtil.readTestQueries();
            expect(testQueries).to.have.length.greaterThan(0);
        } catch (err) {
            expect.fail("", "", `Failed to read one or more test queries. ${JSON.stringify(err)}`);
        }

        try {
            insightFacade = new InsightFacade();
        } catch (err) {
            Log.error(err);
        } finally {
            expect(insightFacade).to.be.instanceOf(InsightFacade);
        }

        // Load the datasets specified in datasetsToQuery and add them to InsightFacade.
        // Fail if there is a problem reading ANY dataset.
        try {
            const loadDatasetPromises: Array<Promise<Buffer>> = [];
            for (const [id, path] of Object.entries(datasetsToQuery)) {
                loadDatasetPromises.push(TestUtil.readFileAsync(path));
            }
            const loadedDatasets = (await Promise.all(loadDatasetPromises)).map((buf, i) => {
                return { [Object.keys(datasetsToQuery)[i]]: buf.toString("base64") };
            });
            expect(loadedDatasets).to.have.length.greaterThan(0);

            const responsePromises: Array<Promise<string[]>> = [];
            const datasets: { [id: string]: string } = Object.assign({}, ...loadedDatasets);
            for (const [id, content] of Object.entries(datasets)) {
                responsePromises.push(insightFacade.addDataset(id, content, InsightDatasetKind.Courses));
            }

            // This try/catch is a hack to let your dynamic tests execute even if the addDataset method fails.
            // In D1, you should remove this try/catch to ensure your datasets load successfully before trying
            // to run you queries.
            try {
                const responses: string[][] = await Promise.all(responsePromises);
                responses.forEach((response) => expect(response).to.be.an("array"));
            } catch (err) {
                Log.warn(`Ignoring addDataset errors. For D1, you should allow errors to fail the Before All hook.`);
            }
        } catch (err) {
            expect.fail("", "", `Failed to read one or more datasets. ${JSON.stringify(err)}`);
        }
    });

    beforeEach(function () {
        Log.test(`BeforeTest: ${this.currentTest.title}`);
    });

    after(function () {
        Log.test(`After: ${this.test.parent.title}`);
    });

    afterEach(function () {
        Log.test(`AfterTest: ${this.currentTest.title}`);
    });

    // Dynamically create and run a test for each query in testQueries
    it("Should run test queries", () => {
        describe("Dynamic InsightFacade PerformQuery tests", () => {
            for (const test of testQueries) {
                it(`[${test.filename}] ${test.title}`, async () => {
                    let response: any[];

                    try {
                        response = await insightFacade.performQuery(test.query);
                    } catch (err) {
                        response = err;
                    } finally {
                        if (test.isQueryValid) {
                            expect(response).to.deep.equal(test.result);
                        } else {
                            expect(response).to.be.instanceOf(InsightError);
                        }
                    }
                });
            }
        });
    });
});
