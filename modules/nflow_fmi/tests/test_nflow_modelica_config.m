%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% This test mutates the shared, persisted modelicaConfigure override; running it
% in parallel would let other workers observe the transient bad configuration.
% <--SEQUENTIAL TEST REQUIRED-->
%=============================================================================
% OpenModelica bridge: modelicaInfo / modelicaConfigure. Validates
% the API contract and the configuration round-trip on every machine; where a
% capable OpenModelica is present it also checks the resolved omc path.
%=============================================================================
% Preserve any pre-existing user configuration; start from auto-detect.
prevOmc = modelicaConfigure();
modelicaConfigure('');
%=============================================================================
% modelicaInfo() returns the documented struct shape, always.
info = modelicaInfo();
assert_istrue(isstruct(info));
assert_istrue(isfield(info, 'available'));
assert_istrue(isfield(info, 'capable'));
assert_istrue(isfield(info, 'omc'));
assert_istrue(isfield(info, 'home'));
assert_istrue(isfield(info, 'version'));
assert_istrue(isfield(info, 'reason'));
assert_istrue(islogical(info.available));
assert_istrue(islogical(info.capable));
assert_istrue(ischar(info.omc));
assert_istrue(ischar(info.reason));
% capable implies available (capability is a strict refinement of presence).
assert_istrue(~info.capable || info.available);
% A non-empty reason is always provided.
assert_istrue(~isempty(info.reason));
%=============================================================================
% Configuration round-trip: an authoritative bad override does not resolve, and
% clearing it returns to auto-detect. The bad-path warning is expected -> off.
fakeHome = tempname();
warning('off', 'Nelson:nflow_fmi:modelicaConfigure');
modelicaConfigure(fakeHome);
warning('on', 'Nelson:nflow_fmi:modelicaConfigure');
infoFake = modelicaInfo();
assert_istrue(~infoFake.available); % the bogus override does not resolve
assert_istrue(~isempty(infoFake.reason));

modelicaConfigure(''); % clear -> auto-detect again
infoAuto = modelicaInfo();
assert_isequal(infoAuto.available, info.available);
%=============================================================================
% The auto-detect chain honours the Nelson-scoped environment variable. When it
% points at the same OpenModelica auto-detect would find, resolution matches; a
% bogus value falls through to the rest of the chain (no exclusive lock).
prevEnv = getenv('NELSON_OPENMODELICA_HOME');
if info.capable
  setenv('NELSON_OPENMODELICA_HOME', info.home);
  infoEnv = modelicaInfo();
  assert_istrue(infoEnv.capable);
  assert_isequal(infoEnv.omc, info.omc);
end
setenv('NELSON_OPENMODELICA_HOME', tempname());
infoBogus = modelicaInfo();
assert_isequal(infoBogus.available, info.available); % fell through to auto-detect
setenv('NELSON_OPENMODELICA_HOME', prevEnv);
%=============================================================================
% Where OpenModelica is installed and capable, the resolver returns a concrete
% omc executable and a version string.
if info.capable
  assert_istrue(~isempty(info.omc));
  assert_istrue(isfile(info.omc));
  assert_istrue(~isempty(info.version));
  assert_istrue(~isempty(info.home));
  assert_isequal(modelicaConfigure(), info.omc);
end
%=============================================================================
% Restore the caller's pre-existing configuration.
if isempty(prevOmc)
  modelicaConfigure('');
else
  warning('off', 'Nelson:nflow_fmi:modelicaConfigure');
  modelicaConfigure(prevOmc);
  warning('on', 'Nelson:nflow_fmi:modelicaConfigure');
end
%=============================================================================
