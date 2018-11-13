import Server from "../src/rest/Server";

import InsightFacade from "../src/controller/InsightFacade";
import chai = require("chai");

import chaiHttp = require("chai-http");
import {expect} from "chai";
import Log from "../src/Util";
import * as fs from "fs";
import Response = ChaiHttp.Response;

describe("Facade D3", function () {

    let facade: InsightFacade = null;
    let server: Server = null;
    let URL = "http://127.0.0.1:4321";
    chai.use(chaiHttp);

    before(function () {
        facade = new InsightFacade();
        server = new Server(4321);
        server.start().then(function (success: boolean) {
            // done
        }).catch(function (e) {
           expect.fail();
        });
    });

    after(function () {
        server.stop().then(function (sucess: boolean) {
            // done
        });
    });

    beforeEach(function () {
        // might want to add some process logging here to keep track of what"s going on
        Log.test(`BeforeTest: ${this.currentTest.title}`);
    });

    afterEach(function () {
        // might want to add some process logging here to keep track of what"s going on
        Log.test(`AfterTest: ${this.currentTest.title}`);
    });

    let courses = fs.readFileSync("./test/data/courses.zip");
    let rooms = fs.readFileSync("./test/data/rooms.zip");
    // Hint on how to test PUT requests

    it("PUT test for courses dataset pass", function () {
        try {
            return chai.request(URL)
                .put("/dataset/courses/courses")
                .attach("body", courses, "courses.zip")
                .then(function (res: Response) {
                    // some logging here please!
                    Log.info("successful put");
                    expect(res.status).to.be.equal(200);
                })
                .catch(function (err: any) {
                    // some logging here please!
                    Log.info(err);
                    expect.fail();
                });
        } catch (err) {
            // and some more logging here!
            Log.info(err);
            expect.fail();
        }
    });

    it("PUT test for rooms dataset pass", function () {
        try {
            return chai.request(URL)
                .put("/dataset/rooms/rooms")
                .attach("body", rooms, "rooms.zip")
                .then(function (res: Response) {
                    // some logging here please!
                    Log.info("successful put");
                    expect(res.status).to.be.equal(200);
                })
                .catch(function (err: any) {
                    // some logging here please!
                    Log.info(err);
                    expect.fail();
                });
        } catch (err) {
            // and some more logging here!
            Log.info(err);
            expect.fail();
        }
    });

    it("PUT test for courses dataset fail due to adding twice", function () {
        try {
            return chai.request(URL)
                .put("/dataset/courses/courses")
                .attach("body", courses, "courses.zip")
                .then(function (res: Response) {
                    // some logging here please!
                    Log.info("successful put");
                    expect.fail();
                })
                .catch(function (err: any) {
                    // some logging here please!
                    Log.info(err);
                    expect(err.status).to.be.equal(400);
                });
        } catch (err) {
            // and some more logging here!
            Log.info(err);
            expect.fail();
        }
    });

    it("PUT test for courses dataset pass even different id", function () {
        try {
            return chai.request(URL)
                .put("/dataset/courses2/courses")
                .attach("body", courses, "courses.zip")
                .then(function (res: Response) {
                    // some logging here please!
                    Log.info("successful put");
                    expect(res.status).to.be.equal(200);
                })
                .catch(function (err: any) {
                    // some logging here please!
                    Log.info(err);
                    expect.fail();
                });
        } catch (err) {
            // and some more logging here!
            Log.info(err);
            expect.fail();
        }
    });

    it("PUT test for courses dataset fail due to invalid kind", function () {
        try {
            return chai.request(URL)
                .put("/dataset/courses/DNE")
                .attach("body", courses, "courses.zip")
                .then(function (res: Response) {
                    // some logging here please!
                    Log.info("successful put");
                    expect.fail();
                })
                .catch(function (err: any) {
                    // some logging here please!
                    Log.info(err);
                    expect(err.status).to.be.equal(400);
                });
        } catch (err) {
            // and some more logging here!
            Log.info(err);
            expect.fail();
        }
    });

    it("PUT test for courses dataset fail due to empty id", function () {
        try {
            return chai.request(URL)
                .put("/dataset//courses")
                .attach("body", courses, "courses.zip")
                .then(function (res: Response) {
                    // some logging here please!
                    Log.info("successful put");
                    expect.fail();
                })
                .catch(function (err: any) {
                    // some logging here please!
                    Log.info(err);
                    expect(err.status).to.be.equal(400);
                });
        } catch (err) {
            // and some more logging here!
            Log.info(err);
            expect.fail();
        }
    });

    it("DELETE test for courses dataset pass", function () {
        try {
            return chai.request(URL)
                .del("/dataset/courses")
                .then(function (res: Response) {
                    // some logging here please!
                    Log.info("successful del");
                    expect(res.status).to.be.equal(200);
                })
                .catch(function (err: any) {
                    // some logging here please!
                    Log.info(err);
                    expect.fail();
                });
        } catch (err) {
            // and some more logging here!
            Log.info(err);
            expect.fail();
        }
    });

    it("DELETE test for courses dataset fail due to repeated delete", function () {
        try {
            return chai.request(URL)
                .del("/dataset/courses")
                .then(function (res: Response) {
                    // some logging here please!
                    Log.info("successful del");
                    expect.fail();
                })
                .catch(function (err: any) {
                    // some logging here please!
                    Log.info(err);
                    expect(err.status).to.be.equal(404);
                });
        } catch (err) {
            // and some more logging here!
            Log.info(err);
            expect.fail();
        }
    });

    it("DELETE test for courses dataset fail due to invalid id", function () {
        try {
            return chai.request(URL)
                .del("/dataset/")
                .then(function (res: Response) {
                    // some logging here please!
                    Log.info("successful del");
                    expect.fail();
                })
                .catch(function (err: any) {
                    // some logging here please!
                    Log.info(err);
                    expect(err.status).to.be.equal(400);
                });
        } catch (err) {
            // and some more logging here!
            Log.info(err);
            expect.fail();
        }
    });

    it("GET test for courses dataset pass", function () {
        try {
            return chai.request(URL)
                .get("/datasets")
                .then(function (res: Response) {
                    // some logging here please!
                    Log.info("successful get");
                    expect(res.status).to.be.equal(200);
                })
                .catch(function (err: any) {
                    // some logging here please!
                    Log.info(err);
                    expect.fail();
                });
        } catch (err) {
            // and some more logging here!
            Log.info(err);
            expect.fail();
        }
    });

    it("PUT test for courses dataset pass 2", function () {
        try {
            return chai.request(URL)
                .put("/dataset/courses/courses")
                .attach("body", courses, "courses.zip")
                .then(function (res: Response) {
                    // some logging here please!
                    Log.info("successful put");
                    expect(res.status).to.be.equal(200);
                })
                .catch(function (err: any) {
                    // some logging here please!
                    Log.info(err);
                    expect.fail();
                });
        } catch (err) {
            // and some more logging here!
            Log.info(err);
            expect.fail();
        }
    });

    it("POST test for query pass q2", function () {
        let query = {
            WHERE: {
                OR: [
                    {
                        AND: [
                            {
                                GT: {
                                    courses_avg: 90
                                }
                            },
                            {
                                IS: {
                                    courses_dept: "adhe"
                                }
                            }
                        ]
                    },
                    {
                        EQ: {
                            courses_avg: 95
                        }
                    }
                ]
            },
            OPTIONS: {
                COLUMNS: [
                    "courses_dept",
                    "courses_id",
                    "courses_avg"
                ],
                ORDER: "courses_avg"
            }
        };

        return chai.request(URL)
            .post("/query").send(query)
            .then(function (res: Response) {
                Log.info("successful post");
                expect(res.status).to.be.equal(200);
            })
            .catch(function (err: any) {
                // and some more logging here!
                Log.info(err);
                expect.fail();
            });
    });

    it("POST test for query fail q4", function () {
        let query = {
            WHERE: {
                GT: {
                    courses_avg: 97
                }
            },
            OPTIONS: {
                COLUMNS: [
                    "courses_notakey",
                    "courses_avg"
                ],
                ORDER: "courses_avg"
            }
        };
        return chai.request(URL)
            .post("/query").send(query)
            .then(function (res: Response) {
                Log.info("successful post");
                expect.fail();
            })
            .catch(function (err: any) {
                // and some more logging here!
                Log.info(err);
                expect(err.status).to.be.equal(400);
            });
    });

    it("echo test to improve coverage", function () {
        try {
            return chai.request(URL)
                .get("/echo/hello")
                .then(function (res: Response) {
                    // some logging here please!
                    Log.info("successful echo");
                    expect(res.status).to.be.equal(200);
                })
                .catch(function (err: any) {
                    // some logging here please!
                    Log.info(err);
                    expect.fail();
                });
        } catch (err) {
            // and some more logging here!
            Log.info(err);
            expect.fail();
        }
    });

    it("echo test fail to improve coverage", function () {
        try {
            return chai.request(URL)
                .get("/echo")
                .then(function (res: Response) {
                    // some logging here please!
                    Log.info("successful echo");
                    expect.fail();
                })
                .catch(function (err: any) {
                    // some logging here please!
                    Log.info(err);
                    expect(err.status).to.be.equal(500);
                });
        } catch (err) {
            // and some more logging here!
            Log.info(err);
            expect.fail();
        }
    });
});
