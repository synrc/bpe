-module(bpe).
-compile(export_all).

% Instance Management

start(Process, Tasks) -> ok.
join(Self, Id) -> ok.
amend(Id, Tasks) -> ok.
step(Stage) -> ok.
finish(Id) -> ok.
history(Id) -> ok.
tasks(Id) -> ok.

% Process Schema

create(Process) -> ok.
add_stage(Process,Stage) -> ok.
add_role(Process,Role) -> ok.
assign_role(Stage,Role) -> ok.
delete(Process) -> ok.
