const jsonld = require('jsonld');
const _ = require('lodash');

let xsd_string = 'http://www.w3.org/2001/XMLSchema#string';
let xsd_integer = 'http://www.w3.org/2001/XMLSchema#integer';
let xsd_decimal = 'http://www.w3.org/2001/XMLSchema#decimal';
let base = 'http://www.tudelft.nl/ewi/iuxe#';


const configure = function (config) {
    xsd_string = config['xsdString'];
    xsd_integer = config['xsdInteger'];
    xsd_decimal = config['xsdDecimal'];
    base = config['baseIri'];
};

const tri = function (...listOfTermsAndFuncs) {
    // console.log(`[CONVERT] building triple of `, listOfTermsAndFuncs);
    return _.chain(listOfTermsAndFuncs).chunk(2).map(([f, t]) => f(t)).join(' ').value() + ' . ';
};

const res = function (shortTerm) {
    return `<${base}${shortTerm}>`;
};

const int = function (i) {
    return `"${i}"^^<${xsd_integer}>`;
};

const str = function (s) {
    return `"${s}"`;
};

const dec = function (d) {
    return `"${d}"^^<${xsd_decimal}>`;
};

const extractCore = function (iri) {
    return iri.replace(base, '');
};


const parseTerm = function (term) {
    console.log(`[SPOTIFY-CLIENT] parsing ${term}`);
    
    if (_.isUndefined(term)) {
        console.log(`[SPOTIFY-CLIENT][ERROR] could not parse term '${term}' because it was undefined.`);
        return '_unknown'; 
    }

    if (term.termType === 'NamedNode') {
        return extractCore(term.value);
    } else if (term.termType === 'Literal' && term.datatype && term.datatype.value === xsd_integer) {
        return parseInt(term.value);
    } else if (term.termType === 'Literal' && term.datatype && term.datatype.value === xsd_decimal) {
        return parseFloat(term.value);
    } else if (term.termType === 'Literal' && term.datatype && term.datatype.value === xsd_string) {
        return term.value;
    } else {
        console.error(`[CONVERT][ERROR] could not parse term '${JSON.stringify(term)}' because it was not an expected type.`, term);
        return term.value ? term.value : '_unknown';
    }
};


const parseTriple = function (tripleTerms) {
    console.log(`[CONVERT] parsing triple ${tripleTerms}.`);
    const { subject, predicate, object } = tripleTerms;
    return [parseTerm(subject), parseTerm(predicate), parseTerm(object)];
};


const parseTriples = function (triplesTerms) {
    console.log(`[CONVERT] parsing triples...`);
    return _.map(triplesTerms, parseTriple);
};


const converters = {
    'application/ld+json': {
        'application/ld+json': function (data) {
            console.log(`No conversion needed.`);
            return Promise.resolve(data);
        },
        'text/turtle': function (data) {
            console.log(`Conversion to 'text/turtle'.`);
            return jsonld.toRDF(data, {format: 'application/n-quads'}).catch(err => {
                console.error(`Error converting ${data} to 'text/turtle'.`, err);
                return '';
            });;
        },
        'raw': function (data) {
            return jsonld.toRDF(data).then(parseTriples);
        }
    },
    'text/turtle': {
        'application/ld+json': function (data) {
            console.log(`Conversion to 'application/ld+json'.`);
            return jsonld.fromRDF(data, {format: 'application/n-quads'}).catch(err => {
                console.error(`Error converting ${data} to 'application/ld+json'.`, err);
                return {};
            });
        },
        'text/turtle': function (data) {
            console.log(`No conversion needed.`);
            return Promise.resolve(data);
        },
        'raw': function (data) {
            return jsonld.fromRDF(data, { format: 'application/n-quads' })
                .then(jsonld.toRDF)
                .then(parseTriples);   
        }
    }
};

const convert = function (desiredFDataType, data, dataType) {
    console.log(`Converting from ${dataType} to ${desiredFDataType}`);
    if (converters[dataType] && converters[dataType][desiredFDataType]) {
        return converters[dataType][desiredFDataType](data);
    } else {
        return Promise.reject({ message: `Convert: Unable to convert from ${dataType} to ${desiredFDataType}.` });
    }
};

_.merge(convert, { tri, res, int, str, dec, configure });

module.exports = convert;