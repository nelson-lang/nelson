%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function images = __nelsonc_installer_images__(options, stage, payload)
  images = struct('InstallerIcon', '', 'InstallerLogo', '', 'AddRemoveProgramsIcon', '');
  names = fieldnames(images);
  if ~any(cellfun(@(name) ~ isempty(options.(name)), names)); return; end
  imageRoot = fullfile(stage, 'images');
  __application_archive__('directory', imageRoot);
  assets = fullfile(payload, 'nelson-installer-assets');
  if ~isempty(options.InstallerIcon) || ~isempty(options.AddRemoveProgramsIcon)
    if isfile(assets) || isfolder(assets)
      error('Nelson:nelsonc:installer:applicationFilesCollideNelsonInstaller', 'Application files collide with nelson-installer-assets.');
    end
    __application_archive__('directory', assets);
  end
  for k = 1:numel(names)
    name = names{k};
    if isempty(options.(name)); continue; end
    work = fullfile(imageRoot, name);
    __application_archive__('directory', work);
    if strcmp(name, 'InstallerLogo')
      files = __nelsonc_image_files__(options.(name), work, [290, 112], true);
      images.(name) = files{1};
    else
      [files, sides] = __nelsonc_icon_files__(options.(name), work);
      images.(name) = fullfile(assets, [name, '.ico']);
      write_icon(images.(name), files, sides);
    end
  end
end

function write_icon(filename, files, sides)
  data = cell(1, numel(files));
  for k = 1:numel(files)
    fid = fopen(files{k}, 'rb');
    if fid < 0; error('Nelson:nelsonc:icon:cannotReadGeneratedIcon', 'Cannot read generated icon.'); end
    close = onCleanup(@() fclose(fid));
    data{k} = fread(fid, Inf, '*uint8');
    clear close;
  end
  fid = fopen(filename, 'wb');
  if fid < 0; error('Nelson:nelsonc:icon:cannotCreateInstallerIcon', 'Cannot create installer icon.'); end
  close = onCleanup(@() fclose(fid));
  write(fid, [0, 1, numel(files)], 'uint16');
  offset = 6 + 16 * numel(files);
  for k = 1:numel(files)
    side = mod(sides(k), 256);
    write(fid, [side, side, 0, 0], 'uint8');
    write(fid, [1, 32], 'uint16');
    write(fid, [numel(data{k}), offset], 'uint32');
    offset = offset + numel(data{k});
  end
  for k = 1:numel(files); write(fid, data{k}, 'uint8'); end
  clear close;
end

function write(fid, values, precision)
  if fwrite(fid, values, precision) ~= numel(values)
    error('Nelson:nelsonc:icon:cannotWriteInstallerIcon', 'Cannot write installer icon.');
  end
end
