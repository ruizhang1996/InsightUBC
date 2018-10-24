// A class for parsing room zip file
import {InsightError} from "./IInsightFacade";
import Log from "../Util";
import {Building} from "./Building";
import {Room} from "./Room";
import JSZip = require("jszip");
import parse5 = require("parse5");
import http = require("http");

export class ParseZipRoom {
    private zip: any;
    private content: string;

    public constructor() {
        this.zip = new JSZip();
    }

    public parseZipRoom(content: string): Promise<Building[]> {
        Log.info("in parseZipRoom");
        let t = this;
        this.content = content;  // save content for getAllRooms()
        return new Promise(function (fulfill, reject) {
            t.zip.loadAsync(content, {base64: true})
                .then(function (dataInZip: JSZip) {
                    dataInZip.file("index.htm").async("text").then(function (indexData: any) {
                        t.parseIndexHTM(indexData).then(function (buildings: Building[]) {
                            fulfill(buildings);
                        }).catch(function (e) {
                            reject(new InsightError("parse IndexHTM fail"));
                        });
                    }).catch(function (e) {
                        reject(new InsightError("async fail"));
                    });
                }).catch(function (e: any) {
                    reject(new InsightError("loadAsync fail"));
            });
        });
    }

    private parseIndexHTM(indexHTM: any): Promise<Building []> {
        let t = this;
        return new Promise(function (fulfill, reject) {
            let parse5Node: any = parse5.parse(indexHTM);
            let buildingCollector: Building[] = [];
            t.getAllBuildings(parse5Node, buildingCollector);
            t.completeBuildingWithRooms(buildingCollector).then(function (buildings: Building[]) {
                let buildingHasRoom: boolean = false;
                for (let b of buildings) {
                    if (b.numberOfRooms() !== 0) {
                        buildingHasRoom = true;
                    }
                }
                if (buildingHasRoom) {
                    fulfill(buildings);
                } else {
                    reject(new InsightError("not a single valid building"));
                }
            }).catch(function (e) {
                reject(new InsightError("completeBuildingWithRoom fail"));
            });
        });
    }

    private getAttributeValue(currTD: any) {
        if (currTD.attrs) {
            for (let a of currTD.attrs) {
                if (a["value"]) {
                    return a["value"];
                }
            }
            return null;
        } else {
            return null;
        }
        // return currTD.attrs[0]["value"];
    }

    private getFullname(currTD: any) {
        if (currTD.childNodes) {
            for (let a of currTD.childNodes) {
                if (a.childNodes) {
                    for (let b of a.childNodes) {
                        if (b["value"]) {
                            return b["value"].trim();
                        }
                    }
                }
            }
            return null;
        } else {
            return null;
        }
        // return currTD.childNodes[1].childNodes[0]["value"].trim();
    }

    private getShortname(currTD: any) {
        if (currTD.childNodes) {
            for (let a of currTD.childNodes) {
                if (a["value"]) {
                    return a["value"].trim();
                }
            }
            return null;
        } else {
            return null;
        }
        // return currTD.childNodes[0]["value"].trim();
    }

    private getAddress(currTD: any) {
        if (currTD.childNodes) {
            for (let a of currTD.childNodes) {
                if (a["value"]) {
                    return a["value"].trim();
                }
            }
            return null;
        } else {
            return null;
        }
        // return currTD.childNodes[0]["value"].trim();
    }

    private getLink(currTD: any) {
        if (currTD.childNodes) {
            for (let a of currTD.childNodes) {
                if (a["attrs"]) {
                    for (let b of a["attrs"]) {
                        if (b["value"]) {
                            return b["value"].trim();
                        }
                    }
                }
            }
            return null;
        } else {
            return null;
        }
        // return currTD.childNodes[1]["attrs"][0]["value"].trim();
    }

    private getAllBuildings(currNode: any, buildingCollector: Building[]): void {
        if (currNode.nodeName === "tr" && currNode.parentNode.nodeName === "tbody") {
            let fullName: string = null;
            let shortName: string = null;
            let address: string = null;
            let link: string = null;
            for (let currTD of currNode.childNodes) {
                if (currTD.nodeName === "td") {
                    if (this.getAttributeValue(currTD).match("title")) {
                        fullName = this.getFullname(currTD);
                        link = this.getLink(currTD);
                    }
                    if (this.getAttributeValue(currTD).match("building-code")) {
                        shortName = this.getShortname(currTD);
                    }
                    if (this.getAttributeValue(currTD).match("building-address")) {
                        address = this.getAddress(currTD);
                    }
                }
            }
            if (fullName !== null && shortName != null && address !== null && link !== null) {
                buildingCollector.push(new Building(fullName, shortName, address, link));
            }
        } else {
            if (currNode.childNodes) {
                for (let child of currNode.childNodes) {
                    this.getAllBuildings(child, buildingCollector);
                }
            }
        }
    }

    private completeBuildingWithRooms(buildingCollector: Building[]): Promise<Building[]> {
        let t = this;
        let arrayOfPromises: Array<Promise<Building>> = [];
        return new Promise<Building[]>(function (fulfill, reject) {
            t.zip.loadAsync(t.content, {base64: true})
                .then(function (dataInZip: JSZip) {
                    for (let building of buildingCollector) {
                        arrayOfPromises.push(t.createBuildingPromise(dataInZip, building));
                    }
                    Promise.all(arrayOfPromises).then(function (buildings: Building[]) {
                        fulfill(buildings);
                    }).catch(function () {
                        reject(new InsightError("at least one promise failed"));
                    });
                }).catch(function () {
                reject(new InsightError("loadAsync fail"));
            });
        });
    }

    private createBuildingPromise(dataInZip: JSZip, building: Building): Promise<Building> {
        let t = this;
        return new Promise<Building>(function (fulfill, reject) {
            dataInZip.file(building.getLink().substring(2)).async("text")
                .then(function (roomData: any) {
                    t.parseRooms(roomData, building).then(function (rooms: Room[]) {
                        building.setRooms(rooms);
                        fulfill(building);
                    });
                }).catch(function () {
                reject(new InsightError("async fail"));
            });
        });
    }

    private parseRooms(roomData: any, building: Building): Promise<Room[]> {
        let t = this;
        return new Promise<Room[]>(function (fulfill, reject) {
            let parse5Node: any = parse5.parse(roomData);
            let roomCollector: Array<Promise<Room>> = [];
            t.getAllRooms(parse5Node, roomCollector, building);
            Promise.all(roomCollector).then(function (rooms: Room[]) {
                let result: Room[] = [];
                for (let r of rooms) {
                    if (r !== null) {
                        result.push(r);
                    }
                }
                fulfill(result);
            }).catch(function () {
                reject(new InsightError("at least one promise failed"));
            });
        });
    }

    private getRoomNumber(currTD: any) {
        if (currTD.childNodes) {
            for (let a of currTD.childNodes) {
                if (a.childNodes) {
                    for (let b of a.childNodes) {
                        if (b["value"]) {
                            return b["value"].trim();
                        }
                    }
                }
            }
            return null;
        } else {
            return null;
        }
        // return currTD.childNodes[1].childNodes[0]["value"].trim();
    }

    private getSeats(currTD: any) {
        if (currTD.childNodes) {
            for (let a of currTD.childNodes) {
                if (a["value"]) {
                    return parseInt(a["value"].trim(), 10);
                }
            }
            return null;
        } else {
            return null;
        }
        // return parseInt(currTD.childNodes[0]["value"].trim(), 10);
    }

    private getFurniture(currTD: any) {
        if (currTD.childNodes) {
            for (let a of currTD.childNodes) {
                if (a["value"]) {
                    return a["value"].trim();
                }
            }
            return null;
        } else {
            return null;
        }
        // return currTD.childNodes[0]["value"].trim();
    }

    private getType(currTD: any) {
        if (currTD.childNodes) {
            for (let a of currTD.childNodes) {
                if (a["value"]) {
                    return a["value"].trim();
                }
            }
            return null;
        } else {
            return null;
        }
        // return currTD.childNodes[0]["value"].trim();
    }

    private getHref(currTD: any) {
        if (currTD.childNodes) {
            for (let a of currTD.childNodes) {
                if (a["attrs"]) {
                    for (let b of a["attrs"]) {
                        if (b["value"]) {
                            return b["value"].trim();
                        }
                    }
                }
            }
            return null;
        } else {
            return null;
        }
        // return currTD.childNodes[1]["attrs"][0]["value"].trim();
    }

    private getAllRooms(currNode: any, roomCollector: Array<Promise<Room>>, building: Building): void {
        if (currNode.nodeName === "tr" && currNode.parentNode.nodeName === "tbody") {
            let fullName: string = null;
            let shortName: string = null;
            let roomNumber: string = null;
            let name: string = null;
            let address: string = null;
            let seats: number = null;
            let type: string = null;
            let furniture: string = null;
            let href: string = null;

            for (let currTD of currNode.childNodes) {
                if (currTD.nodeName === "td") {
                    if (this.getAttributeValue(currTD).match("room-number")) {
                        roomNumber = this.getRoomNumber(currTD);
                        fullName = building.getFullname();
                        shortName = building.getShortname();
                        address = building.getAddress();
                        name = shortName + "_" + roomNumber;
                    }
                    if (this.getAttributeValue(currTD).match("room-capacity")) {
                        seats = this.getSeats(currTD);
                    }
                    if (this.getAttributeValue(currTD).match("room-furniture")) {
                        furniture = this.getFurniture(currTD);
                    }
                    if (this.getAttributeValue(currTD).match("room-type")) {
                        type = this.getType(currTD);
                    }
                    if (this.getAttributeValue(currTD).match("field-nothing")) {
                        href = this.getHref(currTD);
                    }
                }
            }
            roomCollector.push(this.createCourse(fullName, shortName, roomNumber, name, address, seats, type,
                furniture, href));
        } else {
            if (currNode.childNodes) {
                for (let child of currNode.childNodes) {
                    this.getAllRooms(child, roomCollector, building);
                }
            }
        }
    }

    private createCourse(fullname: string, shortname: string, roomNumber: string, name: string, address: string,
                         seats: number, type: string, furniture: string, href: string): Promise<Room> {
        let t = this;
        return new Promise<Room>(function (fulfill) {
            t.getGeoResponse(address).then(function (latlon: any) {
                if (latlon !== null && !latlon.error) {
                    fulfill(new Room(fullname, shortname, roomNumber, name, address, latlon.lat, latlon.lon, seats,
                        type, furniture, href));
                } else {
                    fulfill(new Room(fullname, shortname, roomNumber, name, address, null, null, seats,
                        type, furniture, href));
                }
            }).catch(function (e: any) {
                fulfill(new Room(fullname, shortname, roomNumber, name, address, null, null, seats,
                    type, furniture, href));
            });
        });
    }

    private getGeoResponse(address: string): Promise<any> {
        return new Promise<any>(function (fulfill, reject) {
            let url = "http://cs310.ugrad.cs.ubc.ca:11316/api/v1/project_n3v0b_z9y0b/" + encodeURI(address);
            http.get(url, function (response: any) {
                if (!response.error) {
                    let result: any = null;
                    response.on("data", function (data: any) {
                        result = JSON.parse(data);
                    });
                    response.on("end", function () {
                        fulfill(result);
                    });
                } else {
                    reject(new InsightError("error code 404"));
                }
            }).on("error", function (e: any) {
                reject(new InsightError("some error occurred when call get"));
            });
        });
    }
}
