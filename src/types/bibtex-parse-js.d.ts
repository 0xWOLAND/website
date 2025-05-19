declare namespace bibtexParse {
    interface BibTeXEntry {
        entryType: string;
        entryTags: {
            title?: string;
            author?: string;
            year?: string;
            journal?: string;
            booktitle?: string;
            doi?: string;
            url?: string;
            abstract?: string;
            note?: string;
            publisher?: string;
            volume?: string;
            number?: string;
            pages?: string;
            [key: string]: string | undefined;
        };
    }
}

declare module 'bibtex-parse-js' {
    const bibtexParse: {
        toJSON: (bibtex: string) => bibtexParse.BibTeXEntry[];
    };

    export default bibtexParse;
} 