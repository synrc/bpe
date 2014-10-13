-module(log_allow).
-compile(export_all).

% Put modules here which you want to log.
% Call format:
%                wf:info(?MODULE, "~p",[P])

log_modules() -> [
%   n2o_websocket,
    bpe,
    bpe_task,
    bpe_proc
].
