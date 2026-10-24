%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% <--ADV-CLI MODE-->
%=============================================================================
figfileMulti = [tempname(), '.fig'];
f1 = figure('Visible', 'off', 'Tag', 'figOne');
f2 = figure('Visible', 'off', 'Tag', 'figTwo');
savefig([f1; f2], figfileMulti);
close([f1; f2]);
figs = openfig(figfileMulti, 'invisible');
asserts.isequal(numel(figs), 2);
tags = {figs(1).Tag, figs(2).Tag};
asserts.istrue(any(strcmp(tags, 'figOne')));
asserts.istrue(any(strcmp(tags, 'figTwo')));
rmfile(figfileMulti);
%=============================================================================
%=============================================================================
function localAssertReferenceFigFile()
  figfile = [modulepath('graphics', 'tests'), '/savefig_openfig_reference.fig'];
  asserts.istrue(isfile(figfile));
  f = openfig(figfile, 'invisible');
  asserts.isequal(numel(f), 1);
  asserts.istrue(isgraphics(f, 'figure'));
  asserts.isequal(f.FileName, fullpath(figfile));
  asserts.isequal(f.Visible, 'off');
  asserts.isequal(f.Tag, 'referenceFigure');
  asserts.isequal(f.Color, [0.25 0.5 0.75]);

  ax = findobj(f, 'Tag', 'referenceAxes');
  asserts.isequal(numel(ax), 1);
  asserts.isequal(ax.XLim, [0 5]);
  asserts.isequal(ax.YLim, [-2 2]);
  asserts.isequal(ax.Title.String, 'Reference title');
  asserts.isequal(ax.XLabel.String, 'Reference x');
  asserts.isequal(ax.YLabel.String, 'Reference y');

  l = findobj(f, 'Tag', 'referenceLine');
  asserts.isequal(numel(l), 1);
  asserts.isequal(l.XData, [0 1 2 3 4]);
  asserts.isequal(l.YData, [0 1 0 -1 0]);
  asserts.isequal(l.Color, [0 0.5 1]);
  asserts.isequal(l.LineWidth, 3);
  asserts.isequal(l.Marker, 'o');
  asserts.isequal(numel(findobj(f, 'Type', 'legend')), 1);
  asserts.isequal(numel(findobj(f, 'Type', 'colorbar')), 1);
  close(f);
end
%=============================================================================
function localAssertGraphicsObjectsRecursivelyEqual(expectedRoots, actualRoots)
  asserts.isequal(size(expectedRoots), size(actualRoots));
  expectedObjects = localCollectGraphicsObjects(expectedRoots, gobjects(0, 1));
  actualObjects = localCollectGraphicsObjects(actualRoots, gobjects(0, 1));
  asserts.isequal(numel(expectedObjects), numel(actualObjects));
  for k = 1:numel(expectedObjects)
    localAssertGraphicsObjectEqual(expectedObjects(k), actualObjects(k), ...
      expectedObjects, actualObjects);
  end
end
%=============================================================================
function objects = localCollectGraphicsObjects(handles, objects)
  for k = 1:numel(handles)
    h = handles(k);
    if ~isgraphics(h) || isequal(h, groot())
      continue
    end
    if any(objects == h)
      continue
    end
    objects = [objects; h];
    propNames = localTraversalProperties(h);
    for n = 1:numel(propNames)
      value = get(h, propNames{n});
      if localIsGraphicsObjectValue(value)
        objects = localCollectGraphicsObjects(value, objects);
      end
    end
  end
end
%=============================================================================
function localAssertGraphicsObjectEqual(expected, actual, expectedObjects, actualObjects)
  asserts.isequal(get(expected, 'Type'), get(actual, 'Type'));
  propNames = properties(expected);
  asserts.isequal(propNames, properties(actual));
  for n = 1:numel(propNames)
    name = propNames{n};
    if localSkipRecursiveProperty(name)
      continue
    end
    try
      localAssertPropertyValueEqual(name, get(expected, name), get(actual, name), ...
        expectedObjects, actualObjects);
    catch ME
      disp(['recursive property mismatch: ', get(expected, 'Type'), '.', name]);
      rethrow(ME);
    end
  end
end
%=============================================================================
function localAssertPropertyValueEqual(name, expected, actual, expectedObjects, actualObjects)
  if localIsCellCharList(expected) && ischar(actual)
    localAssertCellCharListEqualsChar(expected, actual);
    return
  end
  if ischar(expected) && localIsCellCharList(actual)
    localAssertCellCharListEqualsChar(actual, expected);
    return
  end
  asserts.isequal(class(expected), class(actual));
  asserts.isequal(size(expected), size(actual));
  if localIsGraphicsObjectValue(expected)
    localAssertGraphicsReferenceEqual(expected, actual, expectedObjects, actualObjects);
  elseif iscell(expected)
    for k = 1:numel(expected)
      localAssertPropertyValueEqual(name, expected{k}, actual{k}, expectedObjects, actualObjects);
    end
  elseif isstruct(expected)
    asserts.isequal(fieldnames(expected), fieldnames(actual));
    names = fieldnames(expected);
    for k = 1:numel(expected)
      for n = 1:numel(names)
        localAssertPropertyValueEqual(name, expected(k).(names{n}), actual(k).(names{n}), ...
          expectedObjects, actualObjects);
      end
    end
  elseif isnumeric(expected)
    asserts.isapprox(double(expected), double(actual), 1e-10);
  else
    asserts.isequal(expected, actual);
  end
end
%=============================================================================
function tf = localIsCellCharList(value)
  tf = iscell(value);
  if ~tf
    return
  end
  for k = 1:numel(value)
    if ~ischar(value{k})
      tf = false;
      return
    end
  end
end
%=============================================================================
function localAssertCellCharListEqualsChar(value, reference)
  for k = 1:numel(value)
    asserts.isequal(value{k}, reference);
  end
end
%=============================================================================
function localAssertGraphicsReferenceEqual(expected, actual, expectedObjects, actualObjects)
  for k = 1:numel(expected)
    expectedIndex = localFindGraphicsObject(expected(k), expectedObjects);
    actualIndex = localFindGraphicsObject(actual(k), actualObjects);
    if expectedIndex ~= 0 || actualIndex ~= 0
      asserts.isequal(expectedIndex, actualIndex);
    else
      asserts.isequal(expected(k), actual(k));
    end
  end
end
%=============================================================================
function index = localFindGraphicsObject(h, objects)
  index = 0;
  for k = 1:numel(objects)
    if objects(k) == h
      index = k;
      return
    end
  end
end
%=============================================================================
function tf = localIsGraphicsObjectValue(value)
  tf = false;
  if isempty(value)
    return
  end
  try
    valid = isgraphics(value);
    tf = ~isempty(valid) && all(valid(:));
  catch
    tf = false;
  end
end
%=============================================================================
function tf = localSkipRecursiveProperty(name)
  % XAxis/YAxis/ZAxis expose numeric ruler views that are never handle-equal
  % across two axes (matching expected behavior), so they cannot be compared by
  % handle identity. Their state is covered through the axes XLim/YLim/... props.
  runtimeProperties = {'Number'; 'CurrentObject'; 'XAxis'; 'YAxis'; 'ZAxis'};
  tf = any(strcmp(name, runtimeProperties)) || endsWith(name, 'Fcn');
end
%=============================================================================
function names = localTraversalProperties(h)
  candidates = {'Children'; 'CurrentAxes'; 'CurrentObject'; 'Title'; 'Subtitle'; ...
    'XLabel'; 'YLabel'; 'ZLabel'; 'Toolbar'; 'ContextMenu'};
  props = properties(h);
  names = {};
  for k = 1:numel(candidates)
    if any(strcmp(props, candidates{k}))
      names = [names; candidates{k}];
    end
  end
end
%=============================================================================
