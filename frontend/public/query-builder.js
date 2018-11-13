/**
 * Builds a query object using the current document object model (DOM).
 * Must use the browser's global document object {@link https://developer.mozilla.org/en-US/docs/Web/API/Document}
 * to read DOM information.
 *
 * @returns query object adhering to the query EBNF
 */
CampusExplorer.buildQuery = function() {
    const id = document.getElementsByClassName("nav-item tab active")[0].getAttribute("data-type");
    let panel = document.getElementsByClassName("tab-panel active")[0];
    let query = {};
    query["WHERE"] = buildWhere(panel,id);
    query["OPTIONS"] = buildOption(panel,id);
    const trans = buildTransformation(panel,id);
    if (Object.keys(trans).length > 0 ){
        query["TRANSFORMATION"] = trans;
    }
    return query;
};

function buildWhere(panel,id) {
    let where = {};
    const condition_types = panel.getElementsByClassName("control-group condition-type")[0].getElementsByTagName("input");
    let typecode = -1;
    let type;
    for (var i = 0 ; i < condition_types.length; i++){
        if(condition_types[i].checked){
            typecode = i;
            break;
        }
    }
    switch (typecode){
        case 0 : type = "AND";
        break;
        case 1: type  = "OR";
        break;
        case 2: type = "NOT";
        break;
    }
    let filters = [];
    const conditions = panel.getElementsByClassName("conditions-container")[0].getElementsByClassName("control-group condition");
    for (const cond of conditions){
        let field;
        let operator;
        let term;
        const fields = cond.getElementsByClassName("control fields")[0].getElementsByTagName("option");
        for ( const f of fields){
            if(f.selected){
                field = f.value;
                break;
            }
        }
        const operators = cond.getElementsByClassName("control operators")[0].getElementsByTagName("option");
        for (const op of operators){
            if (op.selected){
                operator = op.value;
                break;
            }
        }
        const value = cond.getElementsByClassName("control term")[0].getElementsByTagName("input")[0].value;
        if(operator === "IS"){
            term = value;
        } else {
            term = Number(value);
        }
        const ID_KEY = id+ "_" + field;
        let filter = {};
        let entry = {};
        entry[ID_KEY] = term;
        filter[operator]= entry;
        if (cond.getElementsByClassName("control not")[0].getElementsByTagName("input")[0].checked){
            filters.push({"NOT": filter});
        } else {
            filters.push(filter);
        }
    }
    if (filters.length === 0) {
        return where;
    } else if (filters.length === 1) {
        if (type !== "NOT"){
            where = filters[0];
        } else {
            where["NOT"] = filters[0];
        }
    } else {
        if (type !== "NOT"){
            where[type] = filters;
        } else {
            let entry = {};
            entry["OR"] = filters;
            where["NOT"] = entry;
        }
    }
    return where;

}

function buildTransformation(panel,id) {
    let transformation = {};
    let groups = [];
    const groupNames = panel.getElementsByClassName("form-group groups")[0].getElementsByTagName("input");
    for (g of groupNames){
        if (g.checked){
            const ID_KEY = id + "_" + g.value;
            groups.push(ID_KEY)
        }
    }
    const applys = panel.getElementsByClassName("control-group transformation");
    const applyRules = [];
    for (rule of applys){
        const term = rule.getElementsByClassName("control term")[0].value;
        let operator;
        let field;
        for ( const op of rule.getElementsByClassName("control operators")[0].getElementsByTagName("option")){
            if (op.selected){
                operator = op.value;
                break;
            }
        }
        for (const f of rule.getElementsByClassName("control fields")[0].getElementsByTagName("option")){
            if (f.selected){
                field = f.value;
            }
        }
        let applyrule = {};
        const ID_KEY = id + "_" + field;
        let entry = {};
        entry[operator] = ID_KEY;
        applyrule[term] = entry;
        applyRules.push(applyrule);
    }
    if ( groups.length !== 0 || applyRules.length !== 0 ){
        transformation["GROUP"] = groups;
        transformation["APPLY"] = applyRules;
    }

    return transformation;
}

function buildOption(panel,id) {
    let options = {};
    let columns = [];
    for (const c of panel.getElementsByClassName("form-group columns")[0].getElementsByClassName("control field")){
        if (c.getElementsByTagName("input")[0].checked){
            const ID_KEY = id + "_" + c.getElementsByTagName("input")[0].value;
            columns.push[ID_KEY];
        }
    }
    for (const c of panel.getElementsByClassName("form-group columns")[0].getElementsByClassName("control transformation")){
        if (c.getElementsByTagName("input")[0].checked){
            columns.push(c.getElementsByTagName("input")[0].value);
        }
    }
    options["COLUMNS"] = columns;
    let orders = [];
    for (const o of panel.getElementsByClassName("form-group order")[0].getElementsByTagName("option")){
        if (o.selected) {
            let ID_KEY;
            if (o.class !== "transformation"){
                ID_KEY = id + "_" + o.value;
            } else {
                ID_KEY = o.value;
            }
            orders.push(ID_KEY);
        }
    }
    if (orders.length === 0 && panel.getElementsByClassName("control descending")[0].
        getElementsByTagName("input")[0].checked === undefined){
        return options;
    }
    let orderObj = {};
    if (panel.getElementsByClassName("control descending")[0].getElementsByTagName("input")[0].checked){
        orderObj["dir"] = "DOWN";
    } else {
        orderObj["dir"] = "UP";
    }
    orderObj["keys"] = orders;
    options["ORDER"] = orderObj;
    return options;
}
