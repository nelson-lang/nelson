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
% A model's StopFcn (a text blob of Nelson commands stored in the .nflow)
% runs in the base workspace when sim() finishes, with the
% result exposed as 'out'. It must fire even when no output is requested, and a
% failing callback must warn without aborting the completed run.
%=============================================================================
model = [modulepath('nflow_engine', 'tests'), '/stopfcn_demo.nflow'];
%=============================================================================
% Fires with no output requested; sees 'out' (records out.tout length).
if isvar('base', 'stopfcn_samples')
  evalin('base', 'clear stopfcn_samples');
end
sim(model);
asserts.istrue(isvar('base', 'stopfcn_samples'));
samples = evalin('base', 'stopfcn_samples');
asserts.istrue(samples > 1);
% 'out' is left in the base workspace, as documented.
asserts.istrue(isvar('base', 'out'));
%=============================================================================
% Fires as well when an output is requested, and the returned struct matches
% the sample count the callback observed.
evalin('base', 'clear stopfcn_samples');
out = sim(model);
asserts.isequal(numel(out.tout), evalin('base', 'stopfcn_samples'));
%=============================================================================
% A failing StopFcn warns but does not abort: the run already completed, so
% sim() still returns the result.
errModel = [modulepath('nflow_engine', 'tests'), '/stopfcn_error.nflow'];
warning('off', 'Nelson:nflow_engine:stopFcn');
out = sim(errModel);
warning('on', 'Nelson:nflow_engine:stopFcn');
asserts.istrue(isfield(out, 'tout'));
asserts.istrue(numel(out.tout) > 1);
%=============================================================================
