%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 4-->
privatePath = [modulepath('modules_manager', 'functions'), '/private'];
addpath(privatePath);
%=============================================================================
arch = computer('arch');
unsupportedArch = 'glnxa64';
if strcmp(arch, unsupportedArch)
  unsupportedArch = 'win64';
end
%=============================================================================
[r, msg] = nmm_is_supported_platform({arch});
asserts.istrue(r);
asserts.isequal(msg, '');
%=============================================================================
[r, msg] = nmm_is_supported_platform({'all'});
asserts.istrue(r);
asserts.isequal(msg, '');
%=============================================================================
[r, msg] = nmm_is_supported_platform({unsupportedArch});
asserts.isfalse(r);
asserts.istrue(contains(msg, unsupportedArch));
%=============================================================================
[r, msg] = nmm_is_supported_platform({'invalid_platform'});
asserts.isfalse(r);
asserts.istrue(contains(msg, 'invalid_platform'));
%=============================================================================
[r, url, tag] = nmm_is_http_repository('https://example.org/module.git#v1.2.3');
asserts.istrue(r);
asserts.isequal(url, 'https://example.org/module.git');
asserts.isequal(tag, 'v1.2.3');
%=============================================================================
[r, url, tag] = nmm_is_http_repository(1);
asserts.isfalse(r);
asserts.isequal(url, '');
asserts.isequal(tag, '');
%=============================================================================
asserts.checkerror('nmm(''l'')', '#1 value or type not managed.', 'Nelson:nmm:UnknownCommand');
%=============================================================================
testRoot = [tempdir(), 'nmm_phase1_validate/'];
if isdir(testRoot)
  rmdir(testRoot, 's');
end
mkdir(testRoot);
validRoot = [testRoot, 'valid/'];
strictRoot = [testRoot, 'strict/'];
invalidRoot = [testRoot, 'invalid/'];
missingHelpRoot = [testRoot, 'missing_help/'];
invalidLicenseRoot = [testRoot, 'invalid_license/'];
invalidUrlRoot = [testRoot, 'invalid_url/'];
mkdir(validRoot);
mkdir(strictRoot);
mkdir(invalidRoot);
mkdir(missingHelpRoot);
mkdir(invalidLicenseRoot);
mkdir(invalidUrlRoot);
writeModuleJson(validRoot, true);
writeModuleJson(strictRoot, true);
writeStrictModuleJson(strictRoot);
writeModuleJson(invalidRoot, false);
writeModuleJson(missingHelpRoot, true);
writeModuleJson(invalidLicenseRoot, true, 'free text license');
writeStrictModuleJson(invalidUrlRoot);
writeModuleStructure(validRoot, true);
writeModuleStructure(strictRoot, true);
writeModuleStructure(invalidRoot, true);
writeModuleStructure(missingHelpRoot, false);
writeModuleStructure(invalidLicenseRoot, true);
writeModuleStructure(invalidUrlRoot, true);
replaceInFile([invalidUrlRoot, 'module.json'], 'https://example.org/nmm_phase1_validate', 'ftp://example.org/nmm_phase1_validate');
asserts.istrue(nmm('validate', validRoot));
warningsReport = nmm('validate', validRoot, '-warnings');
asserts.istrue(warningsReport.ok);
asserts.istrue(hasTextValue(warningsReport.warnings, _('module.json field is missing: description')));
asserts.istrue(hasTextValue(warningsReport.warnings, _('module.json field is missing: repository')));
asserts.istrue(hasTextValue(warningsReport.warnings, _('module help pages are missing.')));
warningsJson = jsondecode(nmm('validate', validRoot, '-warnings', '-json'));
asserts.istrue(warningsJson.ok);
asserts.istrue(hasTextValue(warningsJson.warnings, _('module.json field is missing: repository')));
asserts.istrue(nmm('validate', strictRoot, '-strict'));
validationJson = jsondecode(nmm('validate', invalidRoot, '-json'));
asserts.isfalse(validationJson.ok);
asserts.checkerror(['nmm(''validate'', ''', invalidRoot, ''')'], _('module.json field is missing: license'), 'Nelson:nmm:ModuleJsonFieldMissing');
asserts.checkerror(['nmm(''validate'', ''', missingHelpRoot, ''')'], _('help directory is missing.'), 'Nelson:nmm:HelpDirectoryMissing');
asserts.checkerror(['nmm(''validate'', ''', invalidLicenseRoot, ''')'], _('module.json license must be a SPDX expression.'), 'Nelson:nmm:ModuleJsonLicenseInvalid');
asserts.checkerror(['nmm(''validate'', ''', validRoot, ''', ''-strict'')'], _('module.json field is missing: repository'), 'Nelson:nmm:ModuleJsonFieldMissing');
asserts.checkerror(['nmm(''validate'', ''', invalidUrlRoot, ''', ''-strict'')'], _('module.json field must be an HTTP URL: repository'), 'Nelson:nmm:ModuleJsonUrlInvalid');
rmdir(testRoot, 's');
%=============================================================================
rmpath(privatePath);
%=============================================================================
function writeModuleJson(moduleRoot, withLicense, varargin)
  license = 'BSD-3-Clause OR MIT';
  if nargin == 3
    license = varargin{1};
  end
  lines = [
    "{";
    "  ""module"": ""nmm_phase1_validate"",";
    "  ""title"": ""nmm phase1 validate"",";
    "  ""summary"": ""validation fixture"",";
    "  ""version"": ""1.0.0"",";
    "  ""platforms"": [""all""],";
    "  ""nelson"": "">=1.0.0"",";
    "  ""builtin"": false,";
  ];
  if withLicense
    lines = [lines; ['  "license": "', license, '",']];
  end
  lines = [
    lines;
    "  ""dependencies"": {""nmm_dep"":"">=1.0.0 <2.0.0""}";
    "}"
  ];
  filewrite([moduleRoot, 'module.json'], lines);
end
%=============================================================================
function writeStrictModuleJson(moduleRoot)
  lines = [
    "{";
    "  ""module"": ""nmm_phase1_validate"",";
    "  ""title"": ""nmm phase1 validate"",";
    "  ""summary"": ""validation fixture"",";
    "  ""version"": ""1.0.0"",";
    "  ""platforms"": [""all""],";
    "  ""nelson"": "">=1.0.0"",";
    "  ""builtin"": false,";
    "  ""license"": ""BSD-3-Clause"",";
    "  ""repository"": ""https://example.org/nmm_phase1_validate.git"",";
    "  ""homepage"": ""https://example.org/nmm_phase1_validate"",";
    "  ""keywords"": [""nmm"", ""validation""],";
    "  ""dependencies"": {""nmm_dep"":"">=1.0.0 <2.0.0""}";
    "}"
  ];
  filewrite([moduleRoot, 'module.json'], lines);
end
%=============================================================================
function writeModuleStructure(moduleRoot, withHelp)
  mkdir([moduleRoot, 'etc']);
  mkdir([moduleRoot, 'tests']);
  if withHelp
    mkdir([moduleRoot, 'help']);
    mkdir([moduleRoot, 'help/en_US']);
    mkdir([moduleRoot, 'help/en_US/xml']);
    filewrite([moduleRoot, 'help/en_US/xml/chapter.xml'], '<chapter></chapter>');
  end
  filewrite([moduleRoot, 'builder.m'], '');
  filewrite([moduleRoot, 'etc/startup.m'], '');
  filewrite([moduleRoot, 'etc/finish.m'], '');
  filewrite([moduleRoot, 'tests/test_placeholder.m'], '');
end
%=============================================================================
function replaceInFile(filename, oldText, newText)
  text = fileread(filename);
  filewrite(filename, strrep(text, oldText, newText));
end
%=============================================================================
function tf = hasTextValue(values, expected)
  tf = false;
  if ischar(values)
    tf = strcmp(values, expected);
    return
  end
  if iscell(values)
    for k = 1:length(values)
      if ischar(values{k}) && strcmp(values{k}, expected)
        tf = true;
        return
      end
    end
  end
end
%=============================================================================
