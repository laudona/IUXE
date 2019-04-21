const handle = function (ws) {

    ws.send(JSON.stringify({
        type: 'event',
        event: 'custom_event',
        dataType: 'text/turtle',
        data: '' +
        `@prefix iuxe:  <http://www.tudelft.nl/ewi/iuxe#> . ` +
        `@prefix xsd:   <http://www.w3.org/2001/XMLSchema#> .` +
        ` ` +
        `iuxe:user iuxe:requested iuxe:more_music . `
    }));

    ws.on('message', function(message) { 
        const {action, data, dataType} = JSON.parse(message);
        console.log(`received message ${action} with ${dataType} data:`, data);
    });
};

module.exports = handle;