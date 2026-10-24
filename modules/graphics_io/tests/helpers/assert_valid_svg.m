%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function info = assert_valid_svg(filename, expectedSize, requiredGroups)
  narginchk(1, 3);
  if nargin < 2
    expectedSize = [];
  end
  if nargin < 3
    requiredGroups = {};
  end

  asserts.istrue(isfile(filename));
  document = xmlread(filename);
  root = document.getDocumentElement();
  asserts.isequal(root.getTagName(), "svg");

  width = svgLength(root.getAttribute('width'));
  height = svgLength(root.getAttribute('height'));
  viewBox = str2double(regexp(root.getAttribute('viewBox'), '-?\d+(\.\d+)?', 'match'));
  asserts.isequal(length(viewBox), 4);
  asserts.istrue(width > 0 && height > 0);
  asserts.istrue(viewBox(3) > 0 && viewBox(4) > 0);
  asserts.isapprox(width / height, viewBox(3) / viewBox(4), 1e-3);

  if ~isempty(expectedSize)
    asserts.isequal(size(expectedSize), [1 2]);
    asserts.istrue(all(expectedSize > 0));
    asserts.isapprox(viewBox(3) / viewBox(4), expectedSize(1) / expectedSize(2), 2e-2);
  end

  visibleTags = {'image', 'rect', 'line', 'polyline', 'polygon', 'path', 'circle', 'ellipse', 'text'};
  visibleCount = 0;
  for tag = visibleTags
    visibleCount = visibleCount + document.getElementsByTagName(tag{1}).getLength();
  end
  asserts.istrue(visibleCount > 0);

  if ischar(requiredGroups)
    requiredGroups = {requiredGroups};
  end
  for groupIndex = 1:length(requiredGroups)
    group = strsplit(requiredGroups{groupIndex}, '|');
    found = false;
    for tagIndex = 1:length(group)
      found = found || document.getElementsByTagName(group{tagIndex}).getLength() > 0;
    end
    asserts.istrue(found);
  end

  info = struct();
  info.text = fileread(filename);
  info.width = width;
  info.height = height;
  info.viewBox = viewBox;
  info.visiblePrimitiveCount = visibleCount;
end
%=============================================================================
function value = svgLength(text)
  token = regexp(text, '^-?\d+(\.\d+)?', 'match', 'once');
  asserts.isfalse(isempty(token));
  value = str2double(token);
end
%=============================================================================
