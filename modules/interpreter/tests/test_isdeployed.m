%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('isdeployed'), 0);
asserts.isequal(nargout('isdeployed'), 1);
asserts.isequal(isdeployed(), false);
assert_rejected(@() isdeployed(1));
assert_rejected(@() too_many_outputs());
previous = __nelsonc_environment__('deployment', 'test-application');
cleanup = onCleanup(@() __nelsonc_environment__('deployment', previous));
asserts.isequal(previous, '');
asserts.isequal(__nelsonc_environment__('deployment'), 'test-application');
asserts.isequal(isdeployed(), true);
oldVariable = getenv('NELSONC_EXTRACTION_DIRECTORY');
environmentCleanup = onCleanup(@() setenv('NELSONC_EXTRACTION_DIRECTORY', oldVariable));
setenv('NELSONC_EXTRACTION_DIRECTORY', '');
asserts.isequal(isdeployed(), true);
assert_rejected(@() __nelsonc_environment__('unknown'));
asserts.isequal(isdeployed(), true);
clear cleanup;
asserts.isequal(isdeployed(), false);
setenv('NELSONC_EXTRACTION_DIRECTORY', 'not-a-deployed-process');
asserts.isequal(isdeployed(), false);
clear environmentCleanup;

function assert_rejected(callback)
  rejected = false;
  try
    callback();
  catch
    rejected = true;
  end
  asserts.istrue(rejected);
end

function too_many_outputs()
  [first, second] = isdeployed();
end
