%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('ctfroot'), 0);
asserts.isequal(nargout('ctfroot'), 1);
assert_rejected(@() ctfroot());
directory = tempname();
mkdir(directory);
previous = __nelsonc_environment__('deployment', directory);
cleanup = onCleanup(@() restore(previous, directory));
asserts.isequal(ctfroot(), directory);
asserts.isequal(class(ctfroot()), 'char');
oldVariable = getenv('NELSONC_EXTRACTION_DIRECTORY');
environmentCleanup = onCleanup(@() setenv('NELSONC_EXTRACTION_DIRECTORY', oldVariable));
setenv('NELSONC_EXTRACTION_DIRECTORY', 'unrelated');
asserts.isequal(ctfroot(), directory);
assert_rejected(@() ctfroot(1));
assert_rejected(@() too_many_outputs());
clear environmentCleanup cleanup;
assert_rejected(@() ctfroot());

function restore(previous, directory)
  __nelsonc_environment__('deployment', previous);
  rmdir(directory, 's');
end

function too_many_outputs()
  [first, second] = ctfroot();
end

function assert_rejected(callback)
  rejected = false;
  try
    callback();
  catch
    rejected = true;
  end
  asserts.istrue(rejected);
end
