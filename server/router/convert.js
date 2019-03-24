const jsonld = require('jsonld');


const converters = {
    'application/ld+json': {
        'application/ld+json': function (data) {
            return Promise.resolve(data);
        },
        'text/turtle': function (data) {
            return jsonld.toRDF(data, {format: 'application/n-quads'});
        }
    },
    'text/turtle': {
        'application/ld+json': function (data) {
            return jsonld.fromRDF(data, {format: 'application/n-quads'});
        },
        'text/turtle': function (data) {
            return Promise.resolve(data);
        }
    }
};

const convert = function (desiredFDataType, data, dataType) {
    if (converters[dataType] && converters[dataType][desiredFDataType]) {
        return converters[dataType][desiredFDataType](data);
    } else {
        return Promise.reject({ message: `Convert: Unable to convert from ${dataType} to ${desiredFDataType}.` });
    }
}

module.exports = convert;