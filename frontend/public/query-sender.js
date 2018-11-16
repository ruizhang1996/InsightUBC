/**
 * Receives a query object as parameter and sends it as Ajax request to the POST /query REST endpoint.
 *
 * @param query The query object
 * @returns {Promise} Promise that must be fulfilled if the Ajax request is successful and be rejected otherwise.
 */
CampusExplorer.sendQuery = function(query) {
    return new Promise(function(fulfill, reject) {
        let request = new XMLHttpRequest();
        request.open("POST", "/query", true);
        request.onload = function() {
            if (request.status === 200) {
                fulfill(JSON.parse(request.response));
            } else {
                reject(JSON.parse(request.response))
            }
        };
        request.setRequestHeader("Content-Type", "application/json");
        request.send(JSON.stringify(query));
    });
};
