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
% Block parameter names are checked against the parameters the block type
% declares: a wrong case is an error, an undeclared name warns.
%=============================================================================
bdclose('all');
new_system('m');
%=============================================================================
% add_block: a name differing only in case is rejected and no block is created.
msg = _('Block type ''gain'' has no parameter ''gain''; did you mean ''Gain''? Parameter names are case-sensitive.');
assert_checkerror('add_block(''nflow/math/gain'', ''m/G'', ''gain'', 3)', msg);
assert_checkerror('get_param(''m/G'', ''Gain'')', _('No block named ''G''.'));
%=============================================================================
% The declared name is accepted silently.
lastwarn('');
add_block('nflow/math/gain', 'm/G', 'Gain', 3);
assert_isequal(lastwarn(), '');
assert_isequal(get_param('m/G', 'Gain'), '3');
%=============================================================================
% Generic parameters any block may carry are accepted silently.
lastwarn('');
add_block('nflow/math/gain', 'm/G2', 'Gain', 2, 'SampleTime', 0.1, 'MakeNameUnique', 'on');
assert_isequal(lastwarn(), '');
%=============================================================================
% An undeclared name warns, and the value is still stored.
lastwarn('');
add_block('nflow/math/gain', 'm/G3', 'Gian', 4);
[msg, id] = lastwarn();
assert_isequal(id, 'Nelson:nflow_engine:unknownBlockParameter');
assert_isequal(msg, 'Block type ''gain'' does not declare a parameter ''Gian'': the value is stored but the block may ignore it.');
assert_isequal(get_param('m/G3', 'Gian'), '4');
%=============================================================================
% set_param applies the same check, before anything is changed.
assert_checkerror('set_param(''m/G'', ''gain'', 5)', _('Block type ''gain'' has no parameter ''gain''; did you mean ''Gain''? Parameter names are case-sensitive.'));
assert_isequal(get_param('m/G', 'Gain'), '3');
assert_checkerror('set_param(''m/G'', ''position'', [0 0 10 10])', _('Block type ''gain'' has no parameter ''position''; did you mean ''Position''? Parameter names are case-sensitive.'));
% A parameter the block already carries is not reported again.
lastwarn('');
set_param('m/G3', 'Gian', 6);
assert_isequal(lastwarn(), '');
%=============================================================================
% Subsystems take free-form (mask) parameters: no check.
lastwarn('');
add_block('subsystem', 'm/Sub', 'myParam', 1);
assert_isequal(lastwarn(), '');
%=============================================================================
bdclose('all');
%=============================================================================
