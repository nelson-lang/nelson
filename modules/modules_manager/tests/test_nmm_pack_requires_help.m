%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Packing must refuse an archive that would drop the required help/ directory
% (no 'files' allowlist), rather than ship a .nmz that fails to install.
testRoot = [tempdir(), createGUID(), '/'];
mkdir(testRoot);
%=============================================================================
% help/ excluded without an allowlist: pack refuses and writes nothing
modNoHelp = [testRoot, 'nohelp/'];
buildBaseModule(modNoHelp, 'nohelp');
asserts.istrue(nmm('validate', modNoHelp));
filewrite([modNoHelp, '.nmignore'], sprintf('help/\n'));
destNoHelp = [testRoot, 'out_nohelp/'];
mkdir(destNoHelp);
errId = '';
try
  nmm('pack', modNoHelp, destNoHelp, '-no-tests');
catch packError
  errId = packError.identifier;
end
asserts.isequal(errId, 'Nelson:nmm:PackHelpDirectoryMissing');
asserts.isequal(numel(dir([destNoHelp, '*.nmz'])), 0);
%=============================================================================
% dry-run only warns, it does not fail, so the selection stays inspectable
lastwarn('');
report = nmm('pack', modNoHelp, '-dry-run', '-no-tests');
asserts.istrue(report.dry_run);
[~, warnId] = lastwarn();
asserts.isequal(warnId, 'Nelson:nmm:PackHelpDirectoryMissing');
%=============================================================================
% a module that keeps help/ still packs
modWithHelp = [testRoot, 'withhelp/'];
buildBaseModule(modWithHelp, 'withhelp');
destWithHelp = [testRoot, 'out_withhelp/'];
mkdir(destWithHelp);
package = nmm('pack', modWithHelp, destWithHelp, '-no-tests');
asserts.istrue(isfile(package));
%=============================================================================
rmdir(testRoot, 's');
%=============================================================================
function buildBaseModule(root, name)
  mkdir(root);
  evalc(['nmm(''init'', ''', root, ''', ''module'', ''', name, ''', ''title'', ''T'', ''summary'', ''s'', ''license'', ''MIT'', ''Skeleton'', true);']);
  mkdir([root, 'functions']);
  filewrite([root, 'functions/foo.m'], ['function y = foo()', char(10), '  y = 1;', char(10), 'end']);
end
%=============================================================================
