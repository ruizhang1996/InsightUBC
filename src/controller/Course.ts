// a course object to store a course's data

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

    public numberOfSections(): number {
        return this.sections.length;
    }
}

export class Section {
    private dept: string;
    private id: string;
    private avg: number;
    private instructor: string;
    private title: string;
    private pass: number;
    private fail: number;
    private audit: number;
    private uuid: string;
    private year: number;

    constructor(dept: string, id: string, avg: number, instructor: string, title: string, pass: number,
                fail: number, audit: number, uuid: string, year: number) {
        this.dept = dept;
        this.id = id;
        this.avg = avg;
        this.instructor = instructor;
        this.title = title;
        this.pass = pass;
        this.fail = fail;
        this.audit = audit;
        this.uuid = uuid;
        this.year = year;
    }
}
