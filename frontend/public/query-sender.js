/**
 * Receives a query object as parameter and sends it as Ajax request to the POST /query REST endpoint.
 *
 * @param query The query object
 * @returns {Promise} Promise that must be fulfilled if the Ajax request is successful and be rejected otherwise.
 */
CampusExplorer.sendQuery = function(query) {
    return new Promise(function(fulfill, reject) {
        var request = new XMLHttpRequest();
        request.open("GET", "http://http://127.0.0.1:8080/" + query, true);
        request.onload = function() {
            var result = JSON.parse(request.responseText);
            if ('error' in result) {
                reject(result.error);
            } else {
                fulfill(result);
            }
        };
        request.onerror = function() {
            reject('The request failed')
        };
        request.send();
    });
};
