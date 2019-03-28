const examples = [];


examples.push({
    title: 'spotify.action.seek',
    type: 'action',
    action: 'spotify.action.seek',
    data: `
<http://www.tudelft.nl/ewi/iuxe#spotify> <http://www.tudelft.nl/ewi/iuxe#seek> "5000"^^<http://www.w3.org/2001/XMLSchema#integer> .
`});

examples.push({
    title: 'spotify.action.play',
    type: 'action',
    action: 'spotify.action.play',
    data: `
    <http://www.tudelft.nl/ewi/iuxe#spotify> <http://www.tudelft.nl/ewi/iuxe#play> "spotify:track:1xEV45D5xMSXSHxs7X27Zj" .
`});

examples.push({
    title: 'spotify.action.get',
    type: 'action',
    action: 'spotify.action.get',
    data: `
    <http://www.tudelft.nl/ewi/iuxe#spotify> <http://www.tudelft.nl/ewi/iuxe#get> <http://www.tudelft.nl/ewi/iuxe#devices> .
`});

examples.push({
    title: 'pepper.action.say',
    type: 'action',
    action: 'pepper.action.say',
    data: `
    <http://www.tudelft.nl/ewi/iuxe#spotify> <http://www.tudelft.nl/ewi/iuxe#get> <http://www.tudelft.nl/ewi/iuxe#devices> .
`});


export default examples;