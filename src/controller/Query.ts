export interface InsightQuery {
    WHERE: InsightFilter;
    OPTIONS: InsightOptions;
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
    ORDER?: string;
}
