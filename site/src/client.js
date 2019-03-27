export default function PurringButterClient(name, code, {host=window.location.hostname, port=3001}) {
    console.log('I get called from print.js!');

    return {
      sendEvent(event, data, dataType) {},
      sendActiont(action, data, dataType) {},
      onAction(action, callback) {},
      onAction(action, callback) {}
    }
}