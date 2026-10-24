%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function tf = verLessThan(toolboxName, versionString)
  narginchk(2, 2);
  nargoutchk(0, 1);
  if ~(ischar(toolboxName) || (isstring(toolboxName) && isscalar(toolboxName)))
    error('Nelson:types:wrongArgumentTypeExpected', _('Wrong type for argument #%d: %s expected.'), 1, 'string');
  end
  if ~(ischar(versionString) || (isstring(versionString) && isscalar(versionString)))
    error('Nelson:types:wrongArgumentTypeExpected', _('Wrong type for argument #%d: %s expected.'), 2, 'string');
  end
  toolboxName = char(toolboxName);
  versionString = char(versionString);
  if isempty(regexp(versionString, '^\d+(\.\d+)*$', 'once'))
    error('Nelson:verLessThan:invalidVersion', _('The VERSION string must be of the form ''major[.minor[.revision]]'', such as ''7.0.1''.'));
  end
  installedVersion = getInstalledVersion(toolboxName);
  tf = logical(compareVersions(installedVersion, versionString) < 0);
end
%=============================================================================
function installedVersion = getInstalledVersion(toolboxName)
  % When a toolbox queries the legacy pseudo-toolbox, report a
  % modern release so that historical version gates all
  % resolve to false and toolboxes take their current code paths rather than
  % legacy/broken ones.
  if strcmpi(toolboxName, 'matlab')
    installedVersion = matlabCompatibleVersion();
    return;
  end
  entry = ver(toolboxName);
  if isempty(entry)
    error('Nelson:verLessThan:missingToolbox', _('The toolbox ''%s'' is not installed.'), toolboxName);
  end
  installedVersion = entry(1).Version;
  if isempty(installedVersion)
    error('Nelson:verLessThan:missingToolbox', _('The toolbox ''%s'' does not report a version.'), toolboxName);
  end
end
%=============================================================================
function v = matlabCompatibleVersion()
  % Report version 25.1. Keep in sync with the release gate helper.
  v = '25.1';
end
%=============================================================================
function c = compareVersions(a, b)
  % Return -1 if a < b, 0 if a == b, 1 if a > b (numeric, field by field).
  fieldsA = str2double(strsplit(a, '.'));
  fieldsB = str2double(strsplit(b, '.'));
  n = max(numel(fieldsA), numel(fieldsB));
  fieldsA(end + 1:n) = 0;
  fieldsB(end + 1:n) = 0;
  c = 0;
  for k = 1:n
    if fieldsA(k) < fieldsB(k)
      c = -1;
      return;
    elseif fieldsA(k) > fieldsB(k)
      c = 1;
      return;
    end
  end
end
%=============================================================================
