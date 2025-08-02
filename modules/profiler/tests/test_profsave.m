%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
assert_isequal(nargin('profsave'), 2);
assert_isequal(nargout('profsave'), 0);
%=============================================================================
profile('on')
run('script_to_profile.m');
profile('off')
destdir = [tempdir(), 'profsave_test'];
if isdir(destdir)
  rmdir(destdir, 's');
end
mkdir(destdir);
profsave(profile('info'), destdir)
assert_istrue(isfile([destdir, '/file0.html']));
assert_istrue(isfile([destdir, '/file1.html']));
assert_istrue(isfile([destdir, '/index.html']));
assert_istrue(isfile([destdir, '/highlight.pack.js']));
assert_istrue(isfile([destdir, '/mono-blue.css']));
assert_istrue(isfile([destdir, '/sort.js']));
if isdir(destdir)
  rmdir(destdir, 's');
end
%=============================================================================
