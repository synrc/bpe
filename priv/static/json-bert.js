function clean(r)      { for(var k in r) if(!r[k]) delete r[k]; return r; }
function check_len(x)  { try { return (eval('len'+utf8_arr(x.v[0].v))() == x.v.length) ? true : false }
                         catch (e) { return false; } }

function scalar(data)    {
    var res = undefined;
    switch (typeof data) {
        case 'string': res = bin(data); break; case 'number': res = number(data); break;
        default: console.log('Strange data: ' + data); }
    return res; };
function nil() { return {t: 106, v: undefined}; };

function decode(x) {
    if (x == undefined) {
        return [];
    } if (x % 1 === 0) {
        return x;
    } else if (x.t == 108) {
        var r = []; x.v.forEach(function(y) { r.push(decode(y)) }); return r;
    } else if (x.t == 109) {
        return utf8_arr(x.v);
    } else if (x.t == 104 && check_len(x)) {
        return eval('dec'+x.v[0].v)(x);
    } else if (x.t == 104) {
        var r=[]; x.v.forEach(function(a){r.push(decode(a))});
	return Object.assign({tup:'$'}, r);
    } else return x.v;
}

function encode(x) {
    if (Array.isArray(x)) {
        var r = []; x.forEach(function(y) { r.push(encode(y)) }); return {t:108,v:r};
    } else if (typeof x == 'object') {
        switch (x.tup) {
	case '$': delete x['tup']; var r=[];
    Object.keys(x).map(function(p){return x[p];}).forEach(function(a){r.push(encode(a))});
	return {t:104, v:r};
	default: return eval('enc'+x.tup)(x); }
    } else return scalar(x);
}

function encid_seq(d) {
    var tup = atom('id_seq');
    var thing = 'thing' in d && d.thing ? encode(d.thing) : nil();
    var id = 'id' in d && d.id ? number(d.id) : nil();
    return tuple(tup,thing,id); }

function lenid_seq() { return 3; }
function decid_seq(d) {
    var r={}; r.tup = 'id_seq';
    r.thing = d && d.v[1] ? decode(d.v[1]) : undefined;
    r.id = d && d.v[2] ? d.v[2].v : undefined;
    return clean(r); }

function encit(d) {
    var tup = atom('it');
    var id = 'id' in d && d.id ? number(d.id) : nil();
    return tuple(tup,id); }

function lenit() { return 2; }
function decit(d) {
    var r={}; r.tup = 'it';
    r.id = d && d.v[1] ? d.v[1].v : undefined;
    return clean(r); }

function encite(d) {
    var tup = atom('ite');
    var id = 'id' in d && d.id ? number(d.id) : nil();
    var next = 'next' in d && d.next ? number(d.next) : nil();
    return tuple(tup,id,next); }

function lenite() { return 3; }
function decite(d) {
    var r={}; r.tup = 'ite';
    r.id = d && d.v[1] ? d.v[1].v : undefined;
    r.next = d && d.v[2] ? d.v[2].v : undefined;
    return clean(r); }

function enciter(d) {
    var tup = atom('iter');
    var id = 'id' in d && d.id ? number(d.id) : nil();
    var next = 'next' in d && d.next ? number(d.next) : nil();
    var prev = 'prev' in d && d.prev ? number(d.prev) : nil();
    return tuple(tup,id,next,prev); }

function leniter() { return 4; }
function deciter(d) {
    var r={}; r.tup = 'iter';
    r.id = d && d.v[1] ? d.v[1].v : undefined;
    r.next = d && d.v[2] ? d.v[2].v : undefined;
    r.prev = d && d.v[3] ? d.v[3].v : undefined;
    return clean(r); }

function enckvx(d) {
    var tup = atom('kvx');
    var mod = 'mod' in d && d.mod ? atom(d.mod) : nil();
    var st = 'st' in d && d.st ? atom(d.st) : nil();
    var cx = 'cx' in d && d.cx ? encode(d.cx) : nil();
    return tuple(tup,mod,st,cx); }

function lenkvx() { return 4; }
function deckvx(d) {
    var r={}; r.tup = 'kvx';
    r.mod = d && d.v[1] ? decode(d.v[1]) : undefined;
    r.st = d && d.v[2] ? decode(d.v[2]) : undefined;
    r.cx = d && d.v[3] ? decode(d.v[3]) : undefined;
    return clean(r); }

function enctask(d) {
    var tup = atom('task');
    var name = 'name' in d && d.name ? atom(d.name) : nil();
    var module = 'module' in d && d.module ? atom(d.module) : nil();
    var prompt = []; if ('prompt' in d && d.prompt)
	 { d.prompt.forEach(function(x){
	prompt.push(encode(x))});
	 prompt={t:108,v:prompt}; } else { prompt = nil() };
    var roles = 'roles' in d && d.roles ? bin(d.roles) : nil();
    return tuple(tup,name,module,prompt,roles); }

function lentask() { return 5; }
function dectask(d) {
    var r={}; r.tup = 'task';
    r.name = d && d.v[1] ? d.v[1].v : undefined;
    r.module = d && d.v[2] ? d.v[2].v : undefined;
    r.prompt = [];
	 (d && d.v[3] && d.v[3].v) ?
	 d.v[3].v.forEach(function(x){r.prompt.push(decode(x))}) :
	 r.prompt = undefined;
    r.roles = d && d.v[4] ? utf8_arr(d.v[4].v) : undefined;
    return clean(r); }

function encuserTask(d) {
    var tup = atom('userTask');
    var name = 'name' in d && d.name ? atom(d.name) : nil();
    var module = 'module' in d && d.module ? atom(d.module) : nil();
    var prompt = []; if ('prompt' in d && d.prompt)
	 { d.prompt.forEach(function(x){
	prompt.push(encode(x))});
	 prompt={t:108,v:prompt}; } else { prompt = nil() };
    var roles = 'roles' in d && d.roles ? bin(d.roles) : nil();
    return tuple(tup,name,module,prompt,roles); }

function lenuserTask() { return 5; }
function decuserTask(d) {
    var r={}; r.tup = 'userTask';
    r.name = d && d.v[1] ? d.v[1].v : undefined;
    r.module = d && d.v[2] ? d.v[2].v : undefined;
    r.prompt = [];
	 (d && d.v[3] && d.v[3].v) ?
	 d.v[3].v.forEach(function(x){r.prompt.push(decode(x))}) :
	 r.prompt = undefined;
    r.roles = d && d.v[4] ? utf8_arr(d.v[4].v) : undefined;
    return clean(r); }

function encserviceTask(d) {
    var tup = atom('serviceTask');
    var name = 'name' in d && d.name ? atom(d.name) : nil();
    var module = 'module' in d && d.module ? atom(d.module) : nil();
    var prompt = []; if ('prompt' in d && d.prompt)
	 { d.prompt.forEach(function(x){
	prompt.push(encode(x))});
	 prompt={t:108,v:prompt}; } else { prompt = nil() };
    var roles = 'roles' in d && d.roles ? bin(d.roles) : nil();
    return tuple(tup,name,module,prompt,roles); }

function lenserviceTask() { return 5; }
function decserviceTask(d) {
    var r={}; r.tup = 'serviceTask';
    r.name = d && d.v[1] ? d.v[1].v : undefined;
    r.module = d && d.v[2] ? d.v[2].v : undefined;
    r.prompt = [];
	 (d && d.v[3] && d.v[3].v) ?
	 d.v[3].v.forEach(function(x){r.prompt.push(decode(x))}) :
	 r.prompt = undefined;
    r.roles = d && d.v[4] ? utf8_arr(d.v[4].v) : undefined;
    return clean(r); }

function encreceiveTask(d) {
    var tup = atom('receiveTask');
    var name = 'name' in d && d.name ? atom(d.name) : nil();
    var module = 'module' in d && d.module ? atom(d.module) : nil();
    var prompt = []; if ('prompt' in d && d.prompt)
	 { d.prompt.forEach(function(x){
	prompt.push(encode(x))});
	 prompt={t:108,v:prompt}; } else { prompt = nil() };
    var roles = 'roles' in d && d.roles ? bin(d.roles) : nil();
    return tuple(tup,name,module,prompt,roles); }

function lenreceiveTask() { return 5; }
function decreceiveTask(d) {
    var r={}; r.tup = 'receiveTask';
    r.name = d && d.v[1] ? d.v[1].v : undefined;
    r.module = d && d.v[2] ? d.v[2].v : undefined;
    r.prompt = [];
	 (d && d.v[3] && d.v[3].v) ?
	 d.v[3].v.forEach(function(x){r.prompt.push(decode(x))}) :
	 r.prompt = undefined;
    r.roles = d && d.v[4] ? utf8_arr(d.v[4].v) : undefined;
    return clean(r); }

function encmessageEvent(d) {
    var tup = atom('messageEvent');
    var name = 'name' in d && d.name ? encode(d.name) : nil();
    var module = 'module' in d && d.module ? atom(d.module) : nil();
    var prompt = []; if ('prompt' in d && d.prompt)
	 { d.prompt.forEach(function(x){
	prompt.push(encode(x))});
	 prompt={t:108,v:prompt}; } else { prompt = nil() };
    var payload = 'payload' in d && d.payload ? bin(d.payload) : nil();
    var timeout = 'timeout' in d && d.timeout ? encode(d.timeout) : nil();
    return tuple(tup,name,module,prompt,payload,timeout); }

function lenmessageEvent() { return 6; }
function decmessageEvent(d) {
    var r={}; r.tup = 'messageEvent';
    r.name = d && d.v[1] ? decode(d.v[1]) : undefined;
    r.module = d && d.v[2] ? d.v[2].v : undefined;
    r.prompt = [];
	 (d && d.v[3] && d.v[3].v) ?
	 d.v[3].v.forEach(function(x){r.prompt.push(decode(x))}) :
	 r.prompt = undefined;
    r.payload = d && d.v[4] ? utf8_arr(d.v[4].v) : undefined;
    r.timeout = d && d.v[5] ? decode(d.v[5]) : undefined;
    return clean(r); }

function encboundaryEvent(d) {
    var tup = atom('boundaryEvent');
    var name = 'name' in d && d.name ? atom(d.name) : nil();
    var module = 'module' in d && d.module ? atom(d.module) : nil();
    var prompt = []; if ('prompt' in d && d.prompt)
	 { d.prompt.forEach(function(x){
	prompt.push(encode(x))});
	 prompt={t:108,v:prompt}; } else { prompt = nil() };
    var payload = 'payload' in d && d.payload ? bin(d.payload) : nil();
    var timeout = 'timeout' in d && d.timeout ? encode(d.timeout) : nil();
    var timeDate = 'timeDate' in d && d.timeDate ? bin(d.timeDate) : nil();
    var timeDuration = 'timeDuration' in d && d.timeDuration ? bin(d.timeDuration) : nil();
    var timeCycle = 'timeCycle' in d && d.timeCycle ? bin(d.timeCycle) : nil();
    return tuple(tup,name,module,prompt,payload,timeout,timeDate,timeDuration,timeCycle); }

function lenboundaryEvent() { return 9; }
function decboundaryEvent(d) {
    var r={}; r.tup = 'boundaryEvent';
    r.name = d && d.v[1] ? d.v[1].v : undefined;
    r.module = d && d.v[2] ? d.v[2].v : undefined;
    r.prompt = [];
	 (d && d.v[3] && d.v[3].v) ?
	 d.v[3].v.forEach(function(x){r.prompt.push(decode(x))}) :
	 r.prompt = undefined;
    r.payload = d && d.v[4] ? utf8_arr(d.v[4].v) : undefined;
    r.timeout = d && d.v[5] ? decode(d.v[5]) : undefined;
    r.timeDate = d && d.v[6] ? utf8_arr(d.v[6].v) : undefined;
    r.timeDuration = d && d.v[7] ? utf8_arr(d.v[7].v) : undefined;
    r.timeCycle = d && d.v[8] ? utf8_arr(d.v[8].v) : undefined;
    return clean(r); }

function enctimeoutEvent(d) {
    var tup = atom('timeoutEvent');
    var name = 'name' in d && d.name ? atom(d.name) : nil();
    var module = 'module' in d && d.module ? atom(d.module) : nil();
    var prompt = []; if ('prompt' in d && d.prompt)
	 { d.prompt.forEach(function(x){
	prompt.push(encode(x))});
	 prompt={t:108,v:prompt}; } else { prompt = nil() };
    var payload = 'payload' in d && d.payload ? bin(d.payload) : nil();
    var timeout = 'timeout' in d && d.timeout ? encode(d.timeout) : nil();
    var timeDate = 'timeDate' in d && d.timeDate ? bin(d.timeDate) : nil();
    var timeDuration = 'timeDuration' in d && d.timeDuration ? bin(d.timeDuration) : nil();
    var timeCycle = 'timeCycle' in d && d.timeCycle ? bin(d.timeCycle) : nil();
    return tuple(tup,name,module,prompt,payload,timeout,timeDate,timeDuration,timeCycle); }

function lentimeoutEvent() { return 9; }
function dectimeoutEvent(d) {
    var r={}; r.tup = 'timeoutEvent';
    r.name = d && d.v[1] ? d.v[1].v : undefined;
    r.module = d && d.v[2] ? d.v[2].v : undefined;
    r.prompt = [];
	 (d && d.v[3] && d.v[3].v) ?
	 d.v[3].v.forEach(function(x){r.prompt.push(decode(x))}) :
	 r.prompt = undefined;
    r.payload = d && d.v[4] ? utf8_arr(d.v[4].v) : undefined;
    r.timeout = d && d.v[5] ? decode(d.v[5]) : undefined;
    r.timeDate = d && d.v[6] ? utf8_arr(d.v[6].v) : undefined;
    r.timeDuration = d && d.v[7] ? utf8_arr(d.v[7].v) : undefined;
    r.timeCycle = d && d.v[8] ? utf8_arr(d.v[8].v) : undefined;
    return clean(r); }

function encbeginEvent(d) {
    var tup = atom('beginEvent');
    var name = 'name' in d && d.name ? atom(d.name) : nil();
    var module = 'module' in d && d.module ? atom(d.module) : nil();
    var prompt = []; if ('prompt' in d && d.prompt)
	 { d.prompt.forEach(function(x){
	prompt.push(encode(x))});
	 prompt={t:108,v:prompt}; } else { prompt = nil() };
    return tuple(tup,name,module,prompt); }

function lenbeginEvent() { return 4; }
function decbeginEvent(d) {
    var r={}; r.tup = 'beginEvent';
    r.name = d && d.v[1] ? d.v[1].v : undefined;
    r.module = d && d.v[2] ? d.v[2].v : undefined;
    r.prompt = [];
	 (d && d.v[3] && d.v[3].v) ?
	 d.v[3].v.forEach(function(x){r.prompt.push(decode(x))}) :
	 r.prompt = undefined;
    return clean(r); }

function encendEvent(d) {
    var tup = atom('endEvent');
    var name = 'name' in d && d.name ? atom(d.name) : nil();
    var module = 'module' in d && d.module ? atom(d.module) : nil();
    var prompt = []; if ('prompt' in d && d.prompt)
	 { d.prompt.forEach(function(x){
	prompt.push(encode(x))});
	 prompt={t:108,v:prompt}; } else { prompt = nil() };
    return tuple(tup,name,module,prompt); }

function lenendEvent() { return 4; }
function decendEvent(d) {
    var r={}; r.tup = 'endEvent';
    r.name = d && d.v[1] ? d.v[1].v : undefined;
    r.module = d && d.v[2] ? d.v[2].v : undefined;
    r.prompt = [];
	 (d && d.v[3] && d.v[3].v) ?
	 d.v[3].v.forEach(function(x){r.prompt.push(decode(x))}) :
	 r.prompt = undefined;
    return clean(r); }

function encsequenceFlow(d) {
    var tup = atom('sequenceFlow');
    var source = 'source' in d && d.source ? atom(d.source) : nil();
    var target = 'target' in d && d.target ? encode(d.target) : nil();
    return tuple(tup,source,target); }

function lensequenceFlow() { return 3; }
function decsequenceFlow(d) {
    var r={}; r.tup = 'sequenceFlow';
    r.source = d && d.v[1] ? d.v[1].v : undefined;
    r.target = d && d.v[2] ? decode(d.v[2]) : undefined;
    return clean(r); }

function enchist(d) {
    var tup = atom('hist');
    var container = 'container' in d && d.container ? atom(d.container) : nil();
    var feed_id = 'feed_id' in d && d.feed_id ? encode(d.feed_id) : nil();
    var prev = 'prev' in d && d.prev ? number(d.prev) : nil();
    var next = 'next' in d && d.next ? number(d.next) : nil();
    var feeds = []; if ('feeds' in d && d.feeds)
	 { d.feeds.forEach(function(x){
	feeds.push(encode(x))});
	 feeds={t:108,v:feeds}; } else { feeds = nil() };
    var name = 'name' in d && d.name ? bin(d.name) : nil();
    var task = 'task' in d && d.task ? encode(d.task) : nil();
    var docs = []; if ('docs' in d && d.docs)
	 { d.docs.forEach(function(x){
	docs.push(encode(x))});
	 docs={t:108,v:docs}; } else { docs = nil() };
    var time = 'time' in d && d.time ? encode(d.time) : nil();
    return tuple(tup,container,feed_id,prev,next,feeds,name,task,docs,time); }

function lenhist() { return 10; }
function dechist(d) {
    var r={}; r.tup = 'hist';
    r.container = d && d.v[1] ? d.v[1].v : undefined;
    r.feed_id = d && d.v[2] ? decode(d.v[2]) : undefined;
    r.prev = d && d.v[3] ? d.v[3].v : undefined;
    r.next = d && d.v[4] ? d.v[4].v : undefined;
    r.feeds = [];
	 (d && d.v[5] && d.v[5].v) ?
	 d.v[5].v.forEach(function(x){r.feeds.push(decode(x))}) :
	 r.feeds = undefined;
    r.name = d && d.v[6] ? utf8_arr(d.v[6].v) : undefined;
    r.task = d && d.v[7] ? decode(d.v[7]) : undefined;
    r.docs = [];
	 (d && d.v[8] && d.v[8].v) ?
	 d.v[8].v.forEach(function(x){r.docs.push(decode(x))}) :
	 r.docs = undefined;
    r.time = d && d.v[9] ? decode(d.v[9]) : undefined;
    return clean(r); }

function encprocess(d) {
    var tup = atom('process');
    var container = 'container' in d && d.container ? atom(d.container) : nil();
    var feed_id = 'feed_id' in d && d.feed_id ? encode(d.feed_id) : nil();
    var prev = 'prev' in d && d.prev ? number(d.prev) : nil();
    var next = 'next' in d && d.next ? number(d.next) : nil();
    var name = 'name' in d && d.name ? encode(d.name) : nil();
    var feeds = []; if ('feeds' in d && d.feeds)
	 { d.feeds.forEach(function(x){
	feeds.push(encode(x))});
	 feeds={t:108,v:feeds}; } else { feeds = nil() };
    var roles = []; if ('roles' in d && d.roles)
	 { d.roles.forEach(function(x){
	roles.push(encode(x))});
	 roles={t:108,v:roles}; } else { roles = nil() };
    var tasks = []; if ('tasks' in d && d.tasks)
	 { d.tasks.forEach(function(x){
	tasks.push(encode(x))});
	 tasks={t:108,v:tasks}; } else { tasks = nil() };
    var events = []; if ('events' in d && d.events)
	 { d.events.forEach(function(x){
	events.push(encode(x))});
	 events={t:108,v:events}; } else { events = nil() };
    var hist = 'hist' in d && d.hist ? encode(d.hist) : nil();
    var flows = []; if ('flows' in d && d.flows)
	 { d.flows.forEach(function(x){
	flows.push(encode(x))});
	 flows={t:108,v:flows}; } else { flows = nil() };
    var rules = 'rules' in d && d.rules ? encode(d.rules) : nil();
    var docs = []; if ('docs' in d && d.docs)
	 { d.docs.forEach(function(x){
	docs.push(encode(x))});
	 docs={t:108,v:docs}; } else { docs = nil() };
    var options = 'options' in d && d.options ? encode(d.options) : nil();
    var task = 'task' in d && d.task ? atom(d.task) : nil();
    var timer = 'timer' in d && d.timer ? encode(d.timer) : nil();
    var notifications = 'notifications' in d && d.notifications ? encode(d.notifications) : nil();
    var result = 'result' in d && d.result ? bin(d.result) : nil();
    var started = 'started' in d && d.started ? encode(d.started) : nil();
    var beginEvent = 'beginEvent' in d && d.beginEvent ? atom(d.beginEvent) : nil();
    var endEvent = 'endEvent' in d && d.endEvent ? atom(d.endEvent) : nil();
    return tuple(tup,container,feed_id,prev,next,name,feeds,roles,tasks,events,hist,
	flows,rules,docs,options,task,timer,notifications,result,started,beginEvent,endEvent); }

function lenprocess() { return 22; }
function decprocess(d) {
    var r={}; r.tup = 'process';
    r.container = d && d.v[1] ? d.v[1].v : undefined;
    r.feed_id = d && d.v[2] ? decode(d.v[2]) : undefined;
    r.prev = d && d.v[3] ? d.v[3].v : undefined;
    r.next = d && d.v[4] ? d.v[4].v : undefined;
    r.name = d && d.v[5] ? decode(d.v[5]) : undefined;
    r.feeds = [];
	 (d && d.v[6] && d.v[6].v) ?
	 d.v[6].v.forEach(function(x){r.feeds.push(decode(x))}) :
	 r.feeds = undefined;
    r.roles = [];
	 (d && d.v[7] && d.v[7].v) ?
	 d.v[7].v.forEach(function(x){r.roles.push(decode(x))}) :
	 r.roles = undefined;
    r.tasks = [];
	 (d && d.v[8] && d.v[8].v) ?
	 d.v[8].v.forEach(function(x){r.tasks.push(decode(x))}) :
	 r.tasks = undefined;
    r.events = [];
	 (d && d.v[9] && d.v[9].v) ?
	 d.v[9].v.forEach(function(x){r.events.push(decode(x))}) :
	 r.events = undefined;
    r.hist = d && d.v[10] ? decode(d.v[10]) : undefined;
    r.flows = [];
	 (d && d.v[11] && d.v[11].v) ?
	 d.v[11].v.forEach(function(x){r.flows.push(decode(x))}) :
	 r.flows = undefined;
    r.rules = d && d.v[12] ? decode(d.v[12]) : undefined;
    r.docs = [];
	 (d && d.v[13] && d.v[13].v) ?
	 d.v[13].v.forEach(function(x){r.docs.push(decode(x))}) :
	 r.docs = undefined;
    r.options = d && d.v[14] ? decode(d.v[14]) : undefined;
    r.task = d && d.v[15] ? d.v[15].v : undefined;
    r.timer = d && d.v[16] ? decode(d.v[16]) : undefined;
    r.notifications = d && d.v[17] ? decode(d.v[17]) : undefined;
    r.result = d && d.v[18] ? utf8_arr(d.v[18].v) : undefined;
    r.started = d && d.v[19] ? decode(d.v[19]) : undefined;
    r.beginEvent = d && d.v[20] ? d.v[20].v : undefined;
    r.endEvent = d && d.v[21] ? d.v[21].v : undefined;
    return clean(r); }

function encComp(d) {
    var tup = atom('Comp');
    var id = 'id' in d && d.id ? number(d.id) : nil();
    return tuple(tup,id); }

function lenComp() { return 2; }
function decComp(d) {
    var r={}; r.tup = 'Comp';
    r.id = d && d.v[1] ? d.v[1].v : undefined;
    return clean(r); }

function encProc(d) {
    var tup = atom('Proc');
    var id = 'id' in d && d.id ? number(d.id) : nil();
    return tuple(tup,id); }

function lenProc() { return 2; }
function decProc(d) {
    var r={}; r.tup = 'Proc';
    r.id = d && d.v[1] ? d.v[1].v : undefined;
    return clean(r); }

function encLoad(d) {
    var tup = atom('Load');
    var id = 'id' in d && d.id ? number(d.id) : nil();
    return tuple(tup,id); }

function lenLoad() { return 2; }
function decLoad(d) {
    var r={}; r.tup = 'Load';
    r.id = d && d.v[1] ? d.v[1].v : undefined;
    return clean(r); }

function encHist(d) {
    var tup = atom('Hist');
    var id = 'id' in d && d.id ? number(d.id) : nil();
    return tuple(tup,id); }

function lenHist() { return 2; }
function decHist(d) {
    var r={}; r.tup = 'Hist';
    r.id = d && d.v[1] ? d.v[1].v : undefined;
    return clean(r); }

function encMake(d) {
    var tup = atom('Make');
    var proc = 'proc' in d && d.proc ? encode(d.proc) : nil();
    var docs = []; if ('docs' in d && d.docs)
	 { d.docs.forEach(function(x){
	docs.push(encode(x))});
	 docs={t:108,v:docs}; } else { docs = nil() };
    return tuple(tup,proc,docs); }

function lenMake() { return 3; }
function decMake(d) {
    var r={}; r.tup = 'Make';
    r.proc = d && d.v[1] ? decode(d.v[1]) : undefined;
    r.docs = [];
	 (d && d.v[2] && d.v[2].v) ?
	 d.v[2].v.forEach(function(x){r.docs.push(decode(x))}) :
	 r.docs = undefined;
    return clean(r); }

function encAmen(d) {
    var tup = atom('Amen');
    var id = 'id' in d && d.id ? number(d.id) : nil();
    var docs = []; if ('docs' in d && d.docs)
	 { d.docs.forEach(function(x){
	docs.push(encode(x))});
	 docs={t:108,v:docs}; } else { docs = nil() };
    return tuple(tup,id,docs); }

function lenAmen() { return 3; }
function decAmen(d) {
    var r={}; r.tup = 'Amen';
    r.id = d && d.v[1] ? d.v[1].v : undefined;
    r.docs = [];
	 (d && d.v[2] && d.v[2].v) ?
	 d.v[2].v.forEach(function(x){r.docs.push(decode(x))}) :
	 r.docs = undefined;
    return clean(r); }

function encmax_tour(d) {
    var tup = atom('max_tour');
    var count = 'count' in d && d.count ? number(d.count) : nil();
    var joined = 'joined' in d && d.joined ? number(d.joined) : nil();
    return tuple(tup,count,joined); }

function lenmax_tour() { return 3; }
function decmax_tour(d) {
    var r={}; r.tup = 'max_tour';
    r.count = d && d.v[1] ? d.v[1].v : undefined;
    r.joined = d && d.v[2] ? d.v[2].v : undefined;
    return clean(r); }

function encjoin_application(d) {
    var tup = atom('join_application');
    var id = 'id' in d && d.id ? number(d.id) : nil();
    var name = 'name' in d && d.name ? encode(d.name) : nil();
    var data = 'data' in d && d.data ? encode(d.data) : nil();
    return tuple(tup,id,name,data); }

function lenjoin_application() { return 4; }
function decjoin_application(d) {
    var r={}; r.tup = 'join_application';
    r.id = d && d.v[1] ? d.v[1].v : undefined;
    r.name = d && d.v[2] ? decode(d.v[2]) : undefined;
    r.data = d && d.v[3] ? decode(d.v[3]) : undefined;
    return clean(r); }

function enctour_list(d) {
    var tup = atom('tour_list');
    var users = []; if ('users' in d && d.users)
	 { d.users.forEach(function(x){
	users.push(encode(x))});
	 users={t:108,v:users}; } else { users = nil() };
    return tuple(tup,users); }

function lentour_list() { return 2; }
function dectour_list(d) {
    var r={}; r.tup = 'tour_list';
    r.users = [];
	 (d && d.v[1] && d.v[1].v) ?
	 d.v[1].v.forEach(function(x){r.users.push(decode(x))}) :
	 r.users = undefined;
    return clean(r); }

function encToken(d) {
    var tup = atom('Token');
    var data = 'data' in d && d.data ? bin(d.data) : nil();
    return tuple(tup,data); }

function lenToken() { return 2; }
function decToken(d) {
    var r={}; r.tup = 'Token';
    r.data = d && d.v[1] ? utf8_arr(d.v[1].v) : undefined;
    return clean(r); }

function encio(d) {
    var tup = atom('io');
    var code = 'code' in d && d.code ? encode(d.code) : nil();
    var data = 'data' in d && d.data ? encode(d.data) : nil();
    return tuple(tup,code,data); }

function lenio() { return 3; }
function decio(d) {
    var r={}; r.tup = 'io';
    r.code = d && d.v[1] ? decode(d.v[1]) : undefined;
    r.data = d && d.v[2] ? decode(d.v[2]) : undefined;
    return clean(r); }

