// a course object to store a course's data
import {Section} from "./Section";
import {InsightDatasetKind} from "./IInsightFacade";

export class Course {
    private courseName: string;
    private sections: Section[];

    constructor(courseName: string) {
        this.courseName = courseName;
        this.sections = [];
    }

    public addSection(section: Section) {
        this.sections.push(section);
    }

    public getType() {
        return InsightDatasetKind.Courses;
    }

    public numberOfSections(): number {
        return this.sections.length;
    }
}
