%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A function in the current directory shadows a function of the same name
% found on the path, and a call re-resolves when the current directory changes
% to another one carrying its own file of that name. The call must agree with
% which(): it must never keep answering with a file left behind by a previous
% current directory.
%=============================================================================
previousDir = pwd();
onPathDir = tempname();
mkdir(onPathDir);
cwdDir = tempname();
mkdir(cwdDir);
otherDir = tempname();
mkdir(otherDir);
% every probe file is written before its directory becomes current, so the
% current-directory index sees it on entry
filewrite(fullfile(onPathDir, 'shadow_probe.m'), ...
  {'function r = shadow_probe()'; '  r = 10;'; 'end'});
filewrite(fullfile(cwdDir, 'shadow_probe.m'), ...
  {'function r = shadow_probe()'; '  r = 20;'; 'end'});
filewrite(fullfile(cwdDir, 'stale_probe.m'), ...
  {'function r = stale_probe()'; '  r = 1;'; 'end'});
filewrite(fullfile(otherDir, 'stale_probe.m'), ...
  {'function r = stale_probe()'; '  r = 2;'; 'end'});
%=============================================================================
% same name on the path and in the current directory: the current one wins
addpath(onPathDir);
cd(cwdDir);
cwdDir = pwd();
% the call resolves to the current directory, not to the path
asserts.isequal(shadow_probe(), 20);
% single which agrees with the first entry of which('-all')
whichSingle = which('shadow_probe');
whichAll = which('shadow_probe', '-all');
asserts.isequal(whichSingle, whichAll{1});
asserts.istrue(endsWith(whichSingle, 'shadow_probe.m'));
rmpath(onPathDir);
%=============================================================================
% leaving a directory for another one defining the same name re-resolves the
% call instead of reusing the definition loaded from the first directory
% loads the current directory copy into memory
asserts.isequal(stale_probe(), 1);
cd(otherDir);
otherDir = pwd();
% the stale copy must not shadow the one in the new current directory
asserts.isequal(stale_probe(), 2);
asserts.istrue(endsWith(which('stale_probe'), 'stale_probe.m'));
%=============================================================================
cd(previousDir);
