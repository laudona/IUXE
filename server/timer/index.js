const _ = require('lodash');
const { tri, res, int, str, dec } = require('../router/convert');


const bind = function ({ interval }, ipAddress, router) {
  const labels = {};
  let labelCount = 1;
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

  const set_id = function (id, label) {
    labels[label] = id;
  }

  const get_id = function (label) {
    return labels[label] || label;
  }

  const send_label_triggered = function(label) {
    return send_event('triggered', tri(res, 'timer', res, 'triggered', str, label), 'text/turtle');
  }

  const send_label_set = function(label) {
    return send_event('set', tri(res, 'timer', res, 'set', str, label), 'text/turtle');
  }

  const send_label_cleared = function(label) {
    return send_event('cleared', tri(res, 'timer', res, 'cleared', str, label), 'text/turtle');
  }

  const set_label = function(func, timeInMs, label) {
    const id = func(() => send_label_triggered(label), timeInMs);
    set_id(id, label);
    return send_label_set(label);
  }

  const clear_label = function(func, label) {
    const id = get_id(label);
    func(id);
    return send_label_cleared(label);
  }


  /**
   * action(timer-set_timeout-"5000/my_label"). 
   * action(timer-set_interval-"5000/my_label").
   * action(timer-clear_interval-<label>). 
   * action(timer-clear_timeout-<label>). 
   * 
   * for the events is does not matter wether it was a timeout or an interval, the events are
   * the same for both.
   * 
   *  events(timer-triggered-<label>).
   *  events(timer-set-<label>).
   *  events(timer-cleared-<label>).
   */
  const actions = {
    'timer': {
      'set_timeout': function (subject, predicate, object, triples) {
        console.log(`[TIMER] set timeout at ${object} ms`);
        const [time, label] = _.isString(object) ? object.split('/') : [ object, "" + labelCount++ ];
        return set_label(setTimeout, parseInt(time), label);
      },
      'set_interval': function (subject, predicate, object, triples) {
        console.log(`[TIMER] set timeout at ${object} ms`);
        const [time, label] = _.isString(object) ? object.split('/') : [ object, "" + labelCount++ ];
        return set_label(setInterval, parseInt(time), label);
      },
      'clear_interval': function (subject, predicate, object, triples) {
        console.log(`[TIMER] clear interval with label '${object}'.`);
        return clear_label(clearInterval, object);
      },
      'clear_timeout': function (subject, predicate, object, triples) {
        console.log(`[TIMER] clear timeout with id ${object}.`);
        return clear_label(clearTimeout, object);
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
    return handle_actions(data).catch(err => send_error(err, action, data, dataType));
  };

  const received_event = function ({ event, data, dataType }) {
    // Nothing maybe? ...
  };

  handler = router.login(router.dummyWs('timer', received_action, received_event), 'mbira', 'LkUItXeMGmjADbAdLOBp');
}

module.exports = bind;