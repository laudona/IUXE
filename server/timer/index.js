const _ = require('lodash');

module.exports = function ({ interval }, ipAddress, router) {
    const eventName = `timer.event.heartbeat`;
    const event = 'heartbeat';
    const name = 'timer';
    const dataType = 'text/turtle';

    let lastTime = Date.now();
    
    setInterval(function() {
        // console.log(`Client '${name}' emits ${eventName} event.`);
        const time = Date.now();
        const delta = time - lastTime;
        lastTime = time;

        let data ='' +
        `@prefix iuxe:  <http://www.tudelft.nl/ewi/iuxe#> . ` +
        `@prefix xsd:   <http://www.w3.org/2001/XMLSchema#> .` +
        ` ` +
        `iuxe:timer iuxe:time "${time}"^^xsd:integer . ` +
        `iuxe:timer iuxe:delta "${delta}"^^xsd:integer . `;

        router.emit(eventName, { event, data, dataType });
  }, interval);
};