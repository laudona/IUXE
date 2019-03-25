const jsonld = require('jsonld');


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
}

module.exports = convert;