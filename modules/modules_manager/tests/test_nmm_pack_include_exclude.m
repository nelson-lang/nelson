%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% Validates the include/exclude selection of the .nmz packager:
%   (a) default pack drops build/VCS junk, keeps sources and install files;
%   (b) .nmignore patterns are honored;
%   (c) a module.json 'files' allowlist restricts the archive to matches
%       plus the always-kept install files;
%   (d) '-dry-run' returns/prints exactly the set that would be packaged and
%       writes nothing;
%   (e) '-no-tests' drops the tests folder from the archive.
%=============================================================================
testRoot = [tempdir(), createGUID(), '/'];
mkdir(testRoot);
%=============================================================================
% (a) default pack excludes junk, keeps sources and install files
modA = [testRoot, 'case_a/'];
buildBaseModule(modA, 'case_a');
mkdir([modA, 'bin']);
filewrite([modA, 'bin/case_a.dll'], 'binary');
filewrite([modA, 'stray.obj'], 'obj');
mkdir([modA, '.git']);
filewrite([modA, '.git/config'], 'gitcfg');
filewrite([modA, 'CMakeCache.txt'], 'cache');
destA = [testRoot, 'out_a/'];
mkdir(destA);
pkgA = nmm('pack', modA, destA);
asserts.istrue(isfile(pkgA));
filesA = unzipList(pkgA, [testRoot, 'x_a/']);
asserts.istrue(any(strcmp(filesA, 'module.json')));
asserts.istrue(any(strcmp(filesA, 'loader.m')));
asserts.istrue(any(strcmp(filesA, 'module-lock.json')));
asserts.istrue(any(strcmp(filesA, 'functions/foo.m')));
asserts.istrue(any(strcmp(filesA, 'tests/test_case_a.m')));
asserts.isfalse(any(strcmp(filesA, 'bin/case_a.dll')));
asserts.isfalse(any(strcmp(filesA, 'stray.obj')));
asserts.isfalse(any(strcmp(filesA, '.git/config')));
asserts.isfalse(any(strcmp(filesA, 'CMakeCache.txt')));
asserts.isfalse(any(strcmp(filesA, '.nmignore')));
%=============================================================================
% (b) .nmignore is honored
modB = [testRoot, 'case_b/'];
buildBaseModule(modB, 'case_b');
filewrite([modB, 'keep.txt'], 'keep');
filewrite([modB, 'debug.log'], 'log');
mkdir([modB, 'scratch']);
filewrite([modB, 'scratch/tmp.dat'], 'tmp');
filewrite([modB, '.nmignore'], sprintf('# comment line\n\n*.log\nscratch/\n'));
destB = [testRoot, 'out_b/'];
mkdir(destB);
pkgB = nmm('pack', modB, destB);
filesB = unzipList(pkgB, [testRoot, 'x_b/']);
asserts.istrue(any(strcmp(filesB, 'keep.txt')));
asserts.istrue(any(strcmp(filesB, 'functions/foo.m')));
asserts.isfalse(any(strcmp(filesB, 'debug.log')));
asserts.isfalse(any(strcmp(filesB, 'scratch/tmp.dat')));
asserts.isfalse(any(strcmp(filesB, '.nmignore')));
%=============================================================================
% (c) a module.json 'files' allowlist restricts the archive
modC = [testRoot, 'case_c/'];
buildBaseModule(modC, 'case_c');
filewrite([modC, 'README.md'], 'readme');
addFilesAllowlist([modC, 'module.json'], {'functions/*.m'});
asserts.istrue(nmm('validate', modC));
destC = [testRoot, 'out_c/'];
mkdir(destC);
pkgC = nmm('pack', modC, destC);
filesC = unzipList(pkgC, [testRoot, 'x_c/']);
asserts.istrue(any(strcmp(filesC, 'functions/foo.m')));
asserts.istrue(any(strcmp(filesC, 'module.json')));
asserts.istrue(any(strcmp(filesC, 'loader.m')));
asserts.istrue(any(strcmp(filesC, 'module-lock.json')));
asserts.isfalse(any(strcmp(filesC, 'README.md')));
asserts.isfalse(any(strcmp(filesC, 'tests/test_case_c.m')));
asserts.isfalse(any(strcmp(filesC, 'etc/startup.m')));
%=============================================================================
% (d) '-dry-run' returns/prints exactly the packaged set and writes nothing
modD = [testRoot, 'case_d/'];
buildBaseModule(modD, 'case_d');
filewrite([modD, 'stray.obj'], 'obj');
destD = [testRoot, 'out_d/'];
mkdir(destD);
outputText = evalc('report = nmm(''pack'', modD, destD, ''-dry-run'');');
asserts.istrue(isstruct(report));
asserts.istrue(report.dry_run);
asserts.istrue(iscell(report.files));
asserts.istrue(contains(outputText, 'functions/foo.m'));
% nothing was written
entriesD = dir([destD, '*.nmz']);
asserts.isequal(numel(entriesD), 0);
asserts.isfalse(isfile(report.package_file));
% the dry-run list matches, exactly, a real package of the same module
pkgD = nmm('pack', modD, destD);
filesD = unzipList(pkgD, [testRoot, 'x_d/']);
asserts.isequal(sort(report.files(:)'), sort(filesD(:)'));
asserts.isfalse(any(strcmp(report.files, 'stray.obj')));
%=============================================================================
% (e) '-no-tests' drops the tests folder from the archive
modE = [testRoot, 'case_e/'];
buildBaseModule(modE, 'case_e');
destE = [testRoot, 'out_e/'];
mkdir(destE);
pkgE = nmm('pack', modE, destE, '-no-tests');
filesE = unzipList(pkgE, [testRoot, 'x_e/']);
asserts.istrue(any(strcmp(filesE, 'functions/foo.m')));
asserts.istrue(any(strcmp(filesE, 'module.json')));
asserts.isfalse(any(strcmp(filesE, 'tests/test_case_e.m')));
%=============================================================================
% malformed 'files' field is rejected by the schema validator
modF = [testRoot, 'case_f/'];
buildBaseModule(modF, 'case_f');
addRawField([modF, 'module.json'], '"files": [1, 2]');
validateId = '';
try
  nmm('validate', modF);
catch validateError
  validateId = validateError.identifier;
end
asserts.isequal(validateId, 'Nelson:nmm:ModuleJsonFilesInvalid');
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
function addFilesAllowlist(module_json_path, globs)
  data = jsondecode(module_json_path, '-file');
  data.files = globs;
  filewrite(module_json_path, jsonprettyprint(jsonencode(data)));
end
%=============================================================================
function addRawField(module_json_path, rawField)
  txt = fileread(module_json_path);
  idx = max(strfind(txt, '}'));
  head = strtrim(txt(1:idx - 1));
  txt = [head, ',', char(10), '  ', rawField, char(10), '}'];
  filewrite(module_json_path, txt);
end
%=============================================================================
function files = unzipList(package_file, extractDir)
  if isdir(extractDir)
    rmdir(extractDir, 's');
  end
  mkdir(extractDir);
  unzip(package_file, extractDir);
  files = listRelativeFiles(ensureSlash(extractDir), '');
end
%=============================================================================
function files = listRelativeFiles(root, rel)
  files = {};
  if isempty(rel)
    listing = dir(root);
  else
    listing = dir([root, rel]);
  end
  for i = 1:numel(listing)
    nm = listing(i).name;
    if strcmp(nm, '.') || strcmp(nm, '..')
      continue;
    end
    if isempty(rel)
      childRel = nm;
    else
      childRel = [rel, '/', nm];
    end
    if listing(i).isdir
      files = [files, listRelativeFiles(root, childRel)];
    else
      files{end + 1} = childRel;
    end
  end
end
%=============================================================================
function path = ensureSlash(path)
  if ~endsWith(path, '/')
    path = [path, '/'];
  end
end
%=============================================================================
