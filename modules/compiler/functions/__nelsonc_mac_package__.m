%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function __nelsonc_mac_package__(root, scripts, location, identifier, version, output, verbose)
  component = fullfile(fileparts(output), 'component.pkg');
  componentList = fullfile(fileparts(output), 'components.plist');
  filewrite(componentList, ['<?xml version="1.0" encoding="UTF-8"?>', char(10), ...
    '<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN" ', ...
    '"http://www.apple.com/DTDs/PropertyList-1.0.dtd">', char(10), ...
    '<plist version="1.0"><array/></plist>', char(10)]);
  invoke({'/usr/bin/pkgbuild', '--root', root, '--install-location', location, ...
    '--scripts', scripts, '--identifier', identifier, '--version', version, ...
    '--ownership', 'preserve', '--component-plist', componentList, component}, verbose);
  invoke({'/usr/bin/productbuild', '--package', component, output}, verbose);
  if ~isfile(output)
    error('Nelson:nelsonc:installerTool:macOSPackageGenerationProducedNoPackage', 'macOS package generation produced no package.');
  end
end

function invoke(parts, verbose)
  for k = 1:numel(parts); parts{k} = shell_argument(parts{k}); end
  [status, output] = system(strjoin(parts, ' '), 1800);
  if verbose || status ~= 0; fprintf('%s\n', output); end
  if status ~= 0
    error('Nelson:nelsonc:installerTool:macOSPackageGenerationFailed', 'macOS package generation failed with status %d.', status);
  end
end

function value = shell_argument(value)
  quote = char(39);
  value = [quote, strrep(value, quote, char([39 34 39 34 39])), quote];
end
