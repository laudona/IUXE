const ArgumentParser = require('argparse').ArgumentParser;

const parser = new ArgumentParser({
    version: '0.1.1',
    addHelp:true,
    description: 'Purring Butter Server'
});

parser.addArgument(
    [ '-c', '--config' ],
    {
        defaultValue: '../data/config.json',
        help: 'config file to use. default ../data/config.json'
    }
);

parser.addArgument(
    [ '-i', '--ip-address' ],
    {
        defaultValue: 'localhost',
        help: 'Own ip address.'
    }
);

const args = parser.parseArgs();

console.log(`loading config file \'${args['config']}\'...`);

try {
    const config = require(args['config']);
    const Router = require('./router');
    const router = new Router(config);
    const spotify = require('./spotify')(config['spotify'], args['ip_address'], router);
    const server = require('./server')(config['server'], args['ip_address'], router, spotify);
    const socket = require('./socket')(config['socket'], args['ip_address'], router);
} catch (e) {
    console.error(`Error starting server.`, e);
}