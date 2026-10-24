%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function varargout = xmldocchecker(varargin)
    narginchk(0, 1);
    nargoutchk(0, 3);
    varargout = {};
    if nargin == 0
        modules_path = [nelsonroot(), '/modules/'];
        module_list = get_modules_help_list();
        disp(_('Checking XML documentation...'));
        for i = 1:length(module_list)
            module_name = module_list{i};
            for lang = getavailablelanguages()'
                help_path = [modules_path, module_name, '/help/', lang{1}, '/xml/'];
                if isdir(help_path)
                    xmlfiles = dir([help_path, '/*.xml'], '-s');
                    for j = 1:length(xmlfiles)
                        xmlfile = [xmlfiles(j).folder, '/', xmlfiles(j).name];
                        switch nargout()
                        case 0
                            try
                              xmldocchecker(xmlfile);
                              msg = sprintf(_('XML documentation file "%s" is valid.'), xmlfile);
                              disp(msg);
                            catch ME
                                msg = sprintf('XML documentation file "%s" is invalid.', xmlfile);
                                disp(msg);
                                throw(ME);
                            end
                        case 1
                            state = xmldocchecker(xmlfile);
                            if ~state
                                varargout{1} = state;
                                return
                            end
                        case 2
                            [state, errors_detected] = xmldocchecker(xmlfile);
                            if ~state
                                varargout{1} = state;
                                varargout{2} = errors_detected;
                                return
                            end
                        case 3
                            [state, errors_detected, warnings_detected] = xmldocchecker(xmlfile);
                            if ~state
                                varargout{1} = state;
                                varargout{2} = errors_detected;
                                varargout{3} = warnings_detected;
                                return
                            end
                        otherwise
                            varargout = {};
                        end
                    end
                end
            end
        end
        switch nargout()
            case 0
                varargout = {};
            case 1
                varargout{1} = true;
            case 2
                varargout{1} = true;
                varargout{2} = {};
            case 3
                varargout{1} = true;
                varargout{2} = {};
                varargout{3} = {};
            otherwise
                varargout = {};
        end
        return;
    end

    xmldocfilename = varargin{1};
    if isdir(xmldocfilename)
        xmlfiles = dir([xmldocfilename, '/*.xml'], '-s');
        for j = 1:length(xmlfiles)
            xmlfile = [xmlfiles(j).folder, '/', xmlfiles(j).name];
            switch nargout()
            case 0
                xmldocchecker(xmlfile);
            case 1
                state = xmldocchecker(xmlfile);
                if ~state
                    varargout{1} = state;
                    return
                end
            case 2
                [state, errors_detected] = xmldocchecker(xmlfile);
                if ~state
                    varargout{1} = state;
                    varargout{2} = errors_detected;
                    return
                end
            case 3
                [state, errors_detected, warnings_detected] = xmldocchecker(xmlfile);
                if ~state
                    varargout{1} = state;
                    varargout{2} = errors_detected;
                    varargout{3} = warnings_detected;
                    return
                end
            end
        end
        switch nargout()
            case 0
                varargout = {};
            case 1
                varargout{1} = true;
            case 2
                varargout{1} = true;
                varargout{2} = {};
            case 3
                varargout{1} = true;
                varargout{2} = {};
                varargout{3} = {};
            otherwise
                varargout = {};
        end
        return;
    end
    % Check if the file exists
    if ~isfile(xmldocfilename)
        error('Nelson:FileNotFound', 'File ''%s'' not found.', xmldocfilename);
    end
    switch nargout()
        case 0
            [state, errors_detected] = checkXmlDocFile(xmldocfilename);
            if ~state
                error('Nelson:xmldocchecker:validationError', errors_detected{1});
            end
        case 1
            state = checkXmlDocFile(xmldocfilename);
            varargout{1} = state;
        case 2
            [state, errors_detected] = checkXmlDocFile(xmldocfilename);
            varargout = {state, errors_detected};
        case 3
            [state, errors_detected, warnings_detected] = checkXmlDocFile(xmldocfilename);
            varargout = {state, errors_detected, warnings_detected};
        otherwise
            varargout = {};
    end
end
%=============================================================================
function [state, errors_detected, warnings_detected] = checkXmlDocFile(xmldocfilename)
    xsd_xmldoc = [modulepath('help_tools'), '/resources/nelson_help.xsd'];
    [state, errors_detected, warnings_detected] = xmlchecker(xmldocfilename, xsd_xmldoc);
    if ~state
        errors_detected = addXmlDocFilenameToMessages(xmldocfilename, errors_detected);
    end
end
%=============================================================================
function messages = addXmlDocFilenameToMessages(xmldocfilename, messages)
    prefix = sprintf('XML documentation file "%s" is invalid.', xmldocfilename);
    if isempty(messages)
        messages = {prefix};
        return
    end
    if ischar(messages)
        messages = {[prefix, newline(), messages]};
        return
    end
    for k = 1:length(messages)
        messages{k} = [prefix, newline(), messages{k}];
    end
end
%=============================================================================
