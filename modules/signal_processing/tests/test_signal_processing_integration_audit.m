%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
skip_testsuite(~isfile([modulepath('signal_processing'), '/help/en_US/xml/chapter.xml']), 'Help XML sources are not available in this build');
moduleRoot = modulepath('signal_processing');
%=============================================================================
privateLeaks = dir([moduleRoot, '/functions/signal*.m']);
asserts.isequal(numel(privateLeaks), 0);
privateHelpers = dir([moduleRoot, '/functions/private/signal*.m']);
asserts.istrue(numel(privateHelpers) > 0);
%=============================================================================
publicFiles = dir([moduleRoot, '/functions/*.m']);
publicNames = {};
for k = 1:numel(publicFiles)
  [~, name, ~] = fileparts(publicFiles(k).name);
  publicNames{end + 1} = name;
end
publicNames = sort(publicNames);
%=============================================================================
plannedPublicNames = {...
  'upsample', 'fftfilt', 'filtfilt', 'sosfilt', 'filtic', ...
  'freqz', 'phasez', 'grpdelay', 'impz', 'stepz', 'filtord', ...
  'isfir', 'isstable', 'tf2zp', 'tf2sos', 'sos2tf', 'zp2sos', ...
  'sos2zp', 'fir1', 'fir2', 'kaiserord', 'butter', 'buttord', ...
  'cheby1', 'cheb1ord', 'cheby2', 'cheb2ord', 'ellip', 'ellipord', ...
  'upfirdn', 'resample', 'decimate', 'interp', 'periodogram', ...
  'pwelch', 'cpsd', 'mscohere', 'tfestimate', 'spectrogram', ...
  'stft', 'istft', 'rectwin', 'triang', 'kaiser', 'blackmanharris', ...
  'tukeywin', 'gausswin', 'chebwin', 'xcorr', 'xcov', 'cconv', ...
  'hilbert', 'rms', 'peak2peak', 'bandpower', 'meanfreq', 'medfreq', ...
  'snr', 'thd', 'chirp', 'sawtooth', 'square', 'rectpuls', 'tripuls', ...
  'medfilt1', 'sgolay', 'sgolayfilt', 'hampel'};
for k = 1:numel(plannedPublicNames)
  asserts.istrue(any(strcmp(publicNames, plannedPublicNames{k})));
end
%=============================================================================
testFiles = dir([moduleRoot, '/tests/test_*.m']);
allTestText = '';
for k = 1:numel(testFiles)
  allTestText = [allTestText, char(10), fileread([moduleRoot, '/tests/', testFiles(k).name])];
end
for k = 1:numel(plannedPublicNames)
  asserts.istrue(~isempty(strfind(allTestText, plannedPublicNames{k})));
end
%=============================================================================
languages = {'en_US', 'fr_FR'};
for langIndex = 1:numel(languages)
  xmlDir = [moduleRoot, '/help/', languages{langIndex}, '/xml'];
  xmlFiles = getXmlFiles(xmlDir);
  xmlNames = {};
  for k = 1:numel(xmlFiles)
    [~, name, ~] = fileparts(xmlFiles{k});
    xmlNames{end + 1} = name;
  end
  for k = 1:numel(publicNames)
    asserts.istrue(any(strcmp(xmlNames, publicNames{k})));
  end
  for k = 1:numel(plannedPublicNames)
    helpFile = xmlFiles{strcmp(xmlNames, plannedPublicNames{k})};
    helpText = fileread(helpFile);
    asserts.istrue(~isempty(strfind(helpText, ['<keyword>', plannedPublicNames{k}, '</keyword>'])));
  end
  helperHelp = {};
  for k = 1:numel(xmlNames)
    if startsWith(xmlNames{k}, 'signal')
      helperHelp{end + 1} = xmlNames{k};
    end
  end
  asserts.isequal(numel(helperHelp), 0);
end
%=============================================================================
function xmlFiles = getXmlFiles(rootDir)
  xmlFiles = {};
  entries = dir(rootDir);
  for k = 1:numel(entries)
    name = entries(k).name;
    if strcmp(name, '.') || strcmp(name, '..')
      continue;
    end
    path = [rootDir, '/', name];
    if entries(k).isdir
      xmlFiles = [xmlFiles, getXmlFiles(path)];
    else
      [~, ~, ext] = fileparts(name);
      if strcmp(ext, '.xml')
        xmlFiles{end + 1} = path;
      end
    end
  end
end
%=============================================================================
