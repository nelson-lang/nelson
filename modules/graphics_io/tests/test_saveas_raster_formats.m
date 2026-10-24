%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--WEIGHT 2-->
% <--ADV-CLI MODE-->
% <--RELEASE ONLY-->
%=============================================================================
destination = [tempname(), '/'];
mkdir(destination);
figure('Visible', 'off');
plot(1:5, [1, 4, 2, 5, 3]);
formats = {'png', 'jpg', 'gif', 'webp', 'tiff', 'bmp', 'tga', ...
           'pbm', 'pgm', 'ppm', 'pnm', 'pcx'};
referenceSize = [];
for k = 1:length(formats)
  filename = [destination, 'figure.', formats{k}];
  saveas(gcf(), filename);
  asserts.istrue(isfile(filename));
  decoded = imread(filename);
  currentSize = [size(decoded, 1), size(decoded, 2)];
  if isempty(referenceSize)
    referenceSize = currentSize;
  end
  asserts.isequal(currentSize, referenceSize);
end
defaultFile = [destination, 'figure_without_extension'];
saveas(gcf(), defaultFile);
asserts.istrue(isfile([defaultFile, '.png']));
forcedFile = [destination, 'figure_forced'];
saveas(gcf(), forcedFile, 'webp');
asserts.istrue(isfile([forcedFile, '.webp']));
forcedImage = imread([forcedFile, '.webp']);
asserts.isequal([size(forcedImage, 1), size(forcedImage, 2)], referenceSize);
aliasFile = [destination, 'figure_alias'];
saveas(gcf(), aliasFile, 'jpeg');
asserts.istrue(isfile([aliasFile, '.jpg']));
aliasImage = imread([aliasFile, '.jpg']);
asserts.isequal([size(aliasImage, 1), size(aliasImage, 2)], referenceSize);
close(gcf());
%=============================================================================
