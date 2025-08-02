%=============================================================================
% Copyright (c) 2023-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = lookup(varargin)
    % TO DO: move to C++
    narginchk(2, 4);
    nargoutchk(0, 1);
    obj = varargin{1};
    if (~isConfigured(obj))
        error (_('Unable to perform a dictionary lookup when the key and value types are not set.'));
    end
    if nargin == 3
        error(_('Wrong number of input arguments.'));
    end
    key = varargin{2};
    if nargin == 2
        if (~(any(ismember(keys(obj), key))))
            error(_('Key not found in dictionary.'));
        end
        varargout{1} = obj(key);
        return;
    end
    fallbackArgument = varargin{3};
    if (~strcmp(varargin{3}, 'FallbackValue'))
        error(_('Parameter name must be ''FallbackValue''.'));
    end
    fallbackValue = varargin{4};    
    if ~isscalar(fallbackValue)
        error(_('Fallback value must be a scalar.'));
    end
    if all(ismember(keys(obj), key))
        varargout{1} = obj(key);
        return
    end
    [keyType, valueType] = types(obj);
    if ~strcmp(class(fallbackValue), valueType)
       fallbackValue = feval(valueType, fallbackValue);
    end
    V = [];
    for i = 1:numel(key)
        if ismember(key(i), keys(obj))
            value = obj(key(i));
        else
            value = fallbackValue;
        end
        V = [V, value];
    end

    varargout{1} = reshape(V, size(key));
end
%=============================================================================
