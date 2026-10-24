%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ENGLISH IMPOSED-->
%=============================================================================
% Programmatic reroute: nflow_reroute recomputes the wire routes of
% a (sub)system with the router and stores waypoints on each connection.
%=============================================================================
bdclose('all');
new_system('rr');
add_block('nflow/source/sine', 'rr/S');
add_block('nflow/math/gain', 'rr/G');
add_line('rr', 'S/1', 'G/1');
% Arrange then route.
nflow_layout('rr');
nflow_reroute('rr');
%=============================================================================
% Persist and inspect: the connection now carries a waypoint route.
f = [tempname(), '.nflow'];
save_system('rr', f);
m = jsondecode(fileread(f));
c = m.connections;
if iscell(c)
  c1 = c{1};
else
  c1 = c(1);
end
assert_istrue(isfield(c1, 'waypoints'));
assert_istrue(numel(c1.waypoints) >= 2);
bdclose('all');
rmfile(f);
%=============================================================================
% An unknown system path is rejected.
new_system('rr2');
assert_checkerror('nflow_reroute(''rr2/Nope'')', _('No block named ''Nope''.'));
bdclose('all');
%=============================================================================
