// a building object to store a building's data
import {Room} from "./Room";
import {InsightDatasetKind} from "./IInsightFacade";

export class Building {
    private fullname: string;
    private shortname: string;
    private address: string;
    private link: string;
    private rooms: Room[];

    constructor(fullname: string, shortname: string, address: string, link: string) {
        this.fullname = fullname;
        this.shortname = shortname;
        this.address = address;
        this.link = link;
        this.rooms = [];
    }

    public setRooms(rooms: Room[]) {
        this.rooms = rooms;
    }

    public getFullname(): string {
        return this.fullname;
    }

    public getShortname(): string {
        return this.shortname;
    }

    public getAddress(): string {
        return this.address;
    }

    public getLink(): string {
        return this.link;
    }

    public getType() {
        return InsightDatasetKind.Rooms;
    }

    public numberOfRooms(): number {
        return this.rooms.length;
    }
}
