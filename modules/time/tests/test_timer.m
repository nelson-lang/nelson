%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
delete(timerfindall());
%=============================================================================
asserts.isequal(nargin('timer'), -1);
asserts.isequal(nargout('timer'), 1);
asserts.isequal(nargin('timerfind'), -1);
asserts.isequal(nargout('timerfind'), 1);
asserts.isequal(nargin('timerfindall'), -1);
asserts.isequal(nargout('timerfindall'), 1);
%=============================================================================
t = timer();
asserts.isequal(class(t), 'timer');
asserts.istrue(isvalid(t));
R = evalc('display(t)');
asserts.istrue(contains(R, 'Timer Object:'));
asserts.istrue(contains(R, 'Timer Settings'));
asserts.istrue(contains(R, 'Callbacks'));
asserts.isfalse(contains(R, 'Show all properties'));
asserts.isequal(t.Type, 'timer');
asserts.isequal(t.Running, 'off');
asserts.isequal(t.Period, 1);
asserts.isequal(t.StartDelay, 0);
asserts.istrue(isinf(t.TasksToExecute));
asserts.isequal(t.TasksExecuted, 0);
asserts.isequal(t.ExecutionMode, 'singleShot');
asserts.isequal(t.BusyMode, 'drop');
asserts.isequal(t.ObjectVisibility, 'on');
asserts.isequal(t.UserData, []);
asserts.istrue(isnan(t.AveragePeriod));
asserts.istrue(isnan(t.InstantPeriod));
%=============================================================================
set(t, 'Name', 'unit_timer', 'Tag', 'visible_timer', 'Period', 0.0204);
asserts.isequal(get(t, 'Name'), 'unit_timer');
asserts.isequal(t.Tag, 'visible_timer');
asserts.isequal(t.Period, 0.02);
set(t, 'busy', 'Queue', 'execution', 'FixedRate', 'objectvisibility', 'OFF');
asserts.isequal(t.BusyMode, 'queue');
asserts.isequal(t.ExecutionMode, 'fixedRate');
asserts.isequal(t.ObjectVisibility, 'off');
set(t, 'TasksToExecute', 3.8);
asserts.isequal(t.TasksToExecute, 3);
set(t, 'TasksToExecute', Inf);
asserts.istrue(isinf(t.TasksToExecute));
set(t, 'StartDelay', 0.0016);
asserts.isequal(t.StartDelay, 0.002);
%=============================================================================
s = get(t);
asserts.isequal(s.Name, 'unit_timer');
asserts.isequal(s.Type, 'timer');
set(t, s);
asserts.isequal(t.Name, 'unit_timer');
%=============================================================================
asserts.istrue(isprop(t, 'TimerFcn'));
asserts.isfalse(isprop(t, 'MissingProperty'));
asserts.istrue(ismethod(t, 'start'));
asserts.isfalse(ismethod(t, 'missingMethod'));
asserts.istrue(any(strcmp(fieldnames(t), 'TimerFcn')));
asserts.istrue(any(strcmp(properties(t), 'Period')));
asserts.istrue(any(strcmp(methods(t), 'wait')));
%=============================================================================
visibleTimer = timer('Tag', 'visible_search');
hiddenTimer = timer('Tag', 'hidden_search', 'ObjectVisibility', 'off');
asserts.isequal(length(timerfind('Tag', 'visible_search')), 1);
asserts.isequal(length(timerfind('Tag', 'hidden_search')), 0);
asserts.isequal(length(timerfindall('Tag', 'hidden_search')), 1);
asserts.isequal(length(timerfindall(visibleTimer, 'Tag', 'visible_search')), 1);
asserts.isequal(length(timerfindall(get(hiddenTimer))), 1);
%=============================================================================
delete(hiddenTimer);
asserts.isfalse(isvalid(hiddenTimer));
delete(timerfindall());
asserts.isequal(length(timerfindall()), 0);
%=============================================================================
