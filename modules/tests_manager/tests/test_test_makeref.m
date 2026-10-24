%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
skip_if_no_native_unittest_runner();
%=============================================================================
asserts.isequal(nargin('test_makeref'), 1);
asserts.isequal(nargout('test_makeref'), 1);
%=============================================================================
filename = [[tempdir(), ''], 'test_makeref_', createGUID(), '.m'];
[pathstr, basename] = fileparts(filename); %#ok<NLS0004>
refname = [pathstr, '/', basename, '.ref'];
resname = [pathstr, '/', basename, '.res'];
content = {'% <--CLI MODE-->'; ...
  '% <--CHECK REF-->'; ...
  'A = 42'};
try
  filewrite(filename, content);
  status = test_makeref(filename);
  asserts.istrue(status);
  asserts.istrue(isfile(refname));
  ref = fileread(refname);
  asserts.istrue(contains(ref, 'A ='));
  asserts.istrue(contains(ref, '42'));
  output = evalc('status = test_run({filename});');
  asserts.istrue(status);
  asserts.istrue(contains(output, basename));
  asserts.isfalse(isfile(resname));
  rmfile(filename);
  rmfile(refname);
catch ex
  if isfile(filename)
    rmfile(filename);
  end
  if isfile(refname)
    rmfile(refname);
  end
  if isfile(resname)
    rmfile(resname);
  end
  rethrow(ex);
end
%=============================================================================
