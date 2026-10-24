%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
asserts.isequal(nargin('profsave'), 2);
asserts.isequal(nargout('profsave'), 0);
%=============================================================================
profile('on')
run('script_to_profile.m');
profile('off')
destdir = tempname();
mkdir(destdir);
profsave(profile('info'), destdir)
asserts.istrue(isfile([destdir, '/file0.html']));
asserts.istrue(isfile([destdir, '/file1.html']));
asserts.istrue(isfile([destdir, '/index.html']));
asserts.istrue(isfile([destdir, '/highlight.pack.js']));
asserts.istrue(isfile([destdir, '/mono-blue.css']));
asserts.istrue(isfile([destdir, '/sort.js']));
if isdir(destdir)
  rmdir(destdir, 's');
end
%=============================================================================
