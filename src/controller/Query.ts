export interface InsightQuery {
    WHERE: InsightFilter;
    OPTIONS: InsightOptions;
    TRANSFORMATIONS?: InsightTransformation;
}
export interface InsightFilter {
    AND?: InsightFilter[];
    OR?: InsightFilter[];
    LT?: {[key: string]: number};
    GT?: {[key: string]: number};
    EQ?: {[key: string]: number};
    IS?: {[key: string]: string};
    NOT?: InsightFilter;
}
export interface InsightOptions {
    COLUMNS: string[];
    ORDER?: string | InsightOrder;
}
export interface InsightTransformation {
    GROUP: string[];
    APPLY: Array<{[applykey: string]: {[token: string]: string}}>;
}
export interface InsightOrder {
    dir: string;
    keys: string[];
}
