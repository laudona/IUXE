const _ = require('lodash');
const { tri, res, int, str, dec } = require('../router/convert');


const bind = function ({ interval }, ipAddress, router) {
  let handler = router.guest();

  const send_error = function (err, action, data, dataType) {
    console.error(`[ERROR][TIMER] error handling action `, err);

    return handler.event({
      type: 'event',
      event: 'timer.event.has_error',
      data: tri(res, 'timer', res, 'has_error', res, 'other'),
      dataType: 'text/turtle'
    });
  };

  const send_event = function (eventName, data, dataType) {
    const event = { type: 'event', event: eventName, data, dataType }
    handler.event(event);
    return event;
  };

  /**
   * action(timer-set_timeout-5000). %% Activate 'timeout_triggered' event in 5000 ms.
   * action(timer-set_interval-5000). %% Activate 'interval_triggered' event every 5000 ms.
   * action(timer-clear_interval-<id>). %% clears the interval with the id.
   * action(timer-clear_timeout-<id>). %% clears the timeout with the id.
   */
  const actions = {
    'timer': {
      'set_timeout': function (subject, predicate, object, triples) {
        console.log(`[TIMER] set timeout at ${object} ms`);
          const id = setTimeout(() =>
            send_event('timeout_triggered',
              tri(res, 'timer', res, 'timeout_triggered', int, id), 'text/turtle'), object);
          return send_event('timeout_set',
            tri(res, 'timer', res, 'timeout_set', int, id), 'text/turtle')
      },
      'set_interval': function (subject, predicate, object, triples) {
        console.log(`[TIMER] set interval every ${object} ms`);
          const id = setInterval(() =>
            send_event('interval_triggered',
              tri(res, 'timer', res, 'interval_triggered', int, id), 'text/turtle'), object);
          return send_event('interval_set',
            tri(res, 'timer', res, 'interval_set', int, id), 'text/turtle')
      },
      'clear_interval': function (subject, predicate, object, triples) {
        console.log(`[TIMER] clear interval with id ${object}.`);
          clearInterval(object)
          return send_event('interval_cleared',
            tri(res, 'timer', res, 'interval_cleared', int, object), 'text/turtle')
      },
      'clear_timeout': function (subject, predicate, object, triples) {
        console.log(`[TIMER] clear timeout with id ${object}.`);
          clearTimeout(object)
          return send_event('timeout_cleared',
            tri(res, 'timer', res, 'timeout_cleared', int, object), 'text/turtle')
      },
    }
  };

  const action_defined = function (triple) {
    console.log(`[TIMER] action defined ${triple}.`);
    const [subject, predicate, object] = triple;

    if (!actions[subject]) {
      console.log(`[TIMER] subject '${subject}' is not defined for actions.`);
      return false;
    }

    if (!actions[subject][predicate]) {
      console.log(`[TIMER] predicate '${predicate}' is not defined for actions of subject '${subject}'.`);
      return false;
    }

    return true;
  }

  const handle_action = function (triple, triples) {
    console.log(`[TIMER] handling triple ${triple}.`);
    if (action_defined(triple)) {
      const [subject, predicate, object] = triple;
      console.log(`[TIMER] calling ${subject}.${predicate}(${object})...`);
      return actions[subject][predicate](subject, predicate, object, triples);
    } else {
      return Promise.reject({ message: `there is no action defined for ${triple}`, code: 404 });
    }
  };

  const handle_actions = function (triples) {
    console.log(`[TIMER] handling actions for `, triples);
    return Promise.all(_.chain(triples)
      .filter(action_defined)
      .map(t => handle_action(t, triples))
      .value());
  };

  const received_action = function ({ action, data, dataType }) {
    console.log(`[TIMER] received action: `, action);
    extractTriples(data, dataType)
      .then(handle_actions)
      .catch(err => send_error(err, action, data, dataType));
  };

  const received_event = function ({ event, data, dataType }) {
    // Nothing maybe? ...
  };

  handler = router.login(router.dummyWs('timer', received_action, received_event), 'mbira', 'LkUItXeMGmjADbAdLOBp');
}

module.exports = bind;