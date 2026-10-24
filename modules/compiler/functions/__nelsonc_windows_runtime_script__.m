%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function [files, lines] = __nelsonc_windows_runtime_script__(runtime, identity)
  files = {};
  if runtime.Included
    source = strrep(strrep(runtime.Package, '{', '{{'), '"', '""');
    files{end + 1} = ['Source: "', source, '"; Flags: dontcopy nocompression touch'];
  end
  files{end + 1} = 'Source: "{tmp}\application.ini"; DestDir: "{code:InstallTargetDir}\.nelson-install"; ExternalSize: 0; Flags: external ignoreversion';
  lines = [{'[Setup]', ['SetupMutex=Nelson-Application-', identity], '[Code]'}, constant('ApplicationIdentity', identity), ...
    constant('RuntimeFingerprint', runtime.Fingerprint), ...
    constant('RuntimeArchitecture', runtime.Architecture), ...
    constant('RuntimeRegistryKey', ['Software\Nelson\Runtime\', runtime.Architecture, '\', runtime.Fingerprint]), ...
    {'function PrivateRuntimeDirectory: String;', 'begin', ...
    ['  Result := AddBackslash(ExpandConstant(''{app}'')) + ''', strrep(runtime.PrivateDirectory, '''', ''''''), ''';'], ...
    'end;', 'function RuntimeIncluded: Boolean;', 'begin', ...
    ['  Result := ', mat2str(runtime.Included), ';'], 'end;'}];
  root = fullfile(modulepath('compiler'), 'resources');
  lines{end + 1} = fileread(fullfile(root, 'installer-paths.iss'));
  lines{end + 1} = fileread(fullfile(root, 'application-runtime.iss'));
end

function lines = constant(name, value)
  lines = {['function ', name, ': String;'], 'begin', ...
    ['  Result := ''', strrep(value, '''', ''''''), ''';'], 'end;'};
end
