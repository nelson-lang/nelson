%=============================================================================
% Copyright (c) 2026-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
% A minimal unit-quaternion class that overloads * for 3-D rotation.
classdef quaternion
  properties
    w
    x
    y
    z
  end

  methods
    function obj = quaternion(w, x, y, z)
      if nargin == 0
        w = 1;
        x = 0;
        y = 0;
        z = 0;
      end
      obj.w = w;
      obj.x = x;
      obj.y = y;
      obj.z = z;
    end

    function result = mtimes(a, b)
      % Overloaded * : the Hamilton product composes two rotations.
      result = quaternion(...
        a.w * b.w - a.x * b.x - a.y * b.y - a.z * b.z, ...
        a.w * b.x + a.x * b.w + a.y * b.z - a.z * b.y, ...
        a.w * b.y - a.x * b.z + a.y * b.w + a.z * b.x, ...
        a.w * b.z + a.x * b.y - a.y * b.x + a.z * b.w);
    end

    function result = normalize(obj)
      norm = sqrt(obj.w ^ 2 + obj.x ^ 2 + obj.y ^ 2 + obj.z ^ 2);
      if norm == 0
        norm = 1;
      end
      result = quaternion(obj.w / norm, obj.x / norm, obj.y / norm, ...
        obj.z / norm);
    end

    function matrix = rotationMatrix(obj)
      % Convert the unit quaternion to a 3x3 rotation matrix.
      unit = normalize(obj);
      ww = unit.w;
      xx = unit.x;
      yy = unit.y;
      zz = unit.z;
      matrix = [1 - 2 * (yy ^ 2 + zz ^ 2), 2 * (xx * yy - ww * zz), ...
        2 * (xx * zz + ww * yy); ...
        2 * (xx * yy + ww * zz), 1 - 2 * (xx ^ 2 + zz ^ 2), ...
        2 * (yy * zz - ww * xx); ...
        2 * (xx * zz - ww * yy), 2 * (yy * zz + ww * xx), ...
        1 - 2 * (xx ^ 2 + yy ^ 2)];
    end

    function display(obj)
      fprintf('quaternion: %.3f + %.3fi + %.3fj + %.3fk\n', ...
        obj.w, obj.x, obj.y, obj.z);
    end
  end
end
%=============================================================================
