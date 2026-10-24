%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
classdef delaunayTriangulation < triangulation
  properties
    Constraints = []
  end

  methods
    function obj = delaunayTriangulation(varargin)
      if nargin == 0
        obj@triangulation();
        return
      end
      [points, constraints] = parseInputs(varargin{:});
      connectivity = delaunayn(points);
      obj@triangulation(connectivity, points);
      obj.Constraints = constraints;
    end

    function hull = convexHull(obj)
      hull = convhulln(obj.Points);
    end

    function tf = isInterior(obj)
      hull = sort(convhulln(obj.Points), 2);
      faces = sort(freeBoundary(obj), 2);
      tf = true(size(obj.ConnectivityList, 1), 1);
      for k = 1:size(obj.ConnectivityList, 1)
        simplexFaces = sort(localFaces(obj.ConnectivityList(k, :)), 2);
        for j = 1:size(simplexFaces, 1)
          if ismember(simplexFaces(j, :), hull, 'rows') || ismember(simplexFaces(j, :), faces, 'rows')
            tf(k) = false;
            break
          end
        end
      end
    end

    function [vertices, cells] = voronoiDiagram(obj)
      [vertices, cells] = voronoin(obj.Points);
    end
  end
end
%=============================================================================
function [points, constraints] = parseInputs(varargin)
  constraints = [];
  if hasConstraints(varargin{:})
    constraints = varargin{end};
    varargin = varargin(1:end - 1);
  end
  if length(varargin) == 1
    points = double(varargin{1});
  else
    points = [];
    for k = 1:length(varargin)
      points = [points, double(varargin{k}(:))];
    end
  end
end
%=============================================================================
function tf = hasConstraints(varargin)
  tf = false;
  if nargin == 2
    first = varargin{1};
    last = varargin{2};
    tf = isnumeric(first) && size(first, 2) >= 2 && isnumeric(last) && size(last, 2) == 2;
  elseif nargin == 3
    last = varargin{3};
    tf = isvector(varargin{1}) && isvector(varargin{2}) && isnumeric(last) && size(last, 2) == 2;
  end
end
%=============================================================================
function faces = localFaces(simplex)
  nv = length(simplex);
  faces = [];
  for omit = 1:nv
    faces = [faces; simplex(setdiff(1:nv, omit))];
  end
end
%=============================================================================
