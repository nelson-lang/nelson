%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
% A function handle to a local function of a function file must stay callable
% for as long as something holds it, even once `clear functions` has dropped
% that file from memory: the deferred callback used to run on a freed
% definition and take the process down.
%=============================================================================
addpath([modulepath('graphics', 'tests'), '/test_callback/']);
close all
global deferredLocalCallbackLog;
%=============================================================================
deferredLocalCallbackLog = {};
f = deferredLocalCallbackFigure('direct');
clear functions
close all
drawnow();
asserts.isequal(deferredLocalCallbackLog, {'delete:on:ObjectBeingDestroyed'});
%=============================================================================
% same through an anonymous function calling the local function
deferredLocalCallbackLog = {};
f = deferredLocalCallbackFigure('anonymous');
clear functions
close all
drawnow();
asserts.isequal(deferredLocalCallbackLog, {'delete:on:ObjectBeingDestroyed'});
%=============================================================================
% two figures holding handles from two separate loads of the same file
deferredLocalCallbackLog = {};
f1 = deferredLocalCallbackFigure('direct');
clear functions
f2 = deferredLocalCallbackFigure('direct');
clear functions
close all
drawnow();
asserts.isequal(deferredLocalCallbackLog, ...
  {'delete:on:ObjectBeingDestroyed', 'delete:on:ObjectBeingDestroyed'});
%=============================================================================
% the callback still runs when the file is dropped only at shutdown
deferredLocalCallbackLog = {};
f = deferredLocalCallbackFigure('direct');
close all
drawnow();
asserts.isequal(deferredLocalCallbackLog, {'delete:on:ObjectBeingDestroyed'});
%=============================================================================
clear global deferredLocalCallbackLog;
close all
%=============================================================================
