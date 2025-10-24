%=============================================================================
% Copyright (c) 2016-present Allan CORNET (Nelson)
%=============================================================================
% This file is part of Nelson.
%=============================================================================
% LICENCE_BLOCK_BEGIN
% SPDX-License-Identifier: LGPL-3.0-or-later
% LICENCE_BLOCK_END
%=============================================================================
function help(varargin)
    % HELP Display help about Nelson macros and built-in functions.
    %
    %   HELP keyword
    %   Displays help about the specified keyword (macro or built-in function).
    %   If no keyword is provided, a general help message is displayed.
    %   txt = HELP(keyword)
    %   Returns the help text as a string instead of displaying it.
    %   Example:
    %       help('plot')
    %       txt = help('plot')
    %   See also ismacro, isbuiltin, getlanguage, nelsonroot, jsondecode, terminal_size.    
    if nargin == 0
        fprintf([_('Type help ''help'' for more information.'), '\n']);
        return
    end
    keyword = varargin{1};
    if ~(ismacro(keyword) || isbuiltin(keyword))
        warning([_('No help available for "%s".'), '\n'], keyword);
        if nargout > 0
            varargout{1} = '';
        end
        return
    end
    json = [nelsonroot(), filesep(), 'modules', filesep(), 'help_tools', filesep(), 'help', filesep(), 'nelson_help_', getlanguage(), '.json'];

    if ~isfile(json)
        msg = sprintf(_('Help file not found: "%s".'), json);
        warning(msg);
        if nargout > 0
            varargout{1} = '';
        end
        return
    end
    helpData = jsondecode(json, '-file');
    if isempty(helpData)
        msg = sprintf(_('No help available for "%s".'), keyword);
        warning(msg);
        if nargout > 0
            varargout{1} = '';
        end
        return
    end
    if ~isfield(helpData, keyword)
        if ismacro(keyword)
            ce = headcomments(keyword);
            if isempty(ce)
                msg = sprintf(_('No help available for macro "%s".'), keyword);
                warning(msg);
                if nargout > 0
                    varargout{1} = '';
                end
                return
            end
            if nargout == 0
                for l = ce(:)'
                    fprintf([l{1}, newline()]);
                end
            else
                varargout{1} = join(ce, newline());
            end
            return
        else
            msg = sprintf(_('Help not found for "%s".'), keyword);
            warning(msg);
            if nargout > 0
                varargout{1} = '';
            end
            return
        end
    end
    [r, c] = terminal_size();
    txt  = formatStruct(helpData.(keyword), c - 4);
    if nargout == 0
        fprintf(txt);
    else
        varargout{1} = txt;
    end 
end
%=============================================================================
function txt = formatStruct(st, maxLineLength)
    txt = '';
    if isempty(st)
        return
    end
    if isfield(st, 'short_description')
        r = formatShortDescription(st, maxLineLength);
        if ~isempty(r)
            txt = [txt, r, newline];
        end
    end
    if isfield(st, 'description')
        r = formatDescription(st, maxLineLength);
        if ~isempty(r)
            if ~isempty(txt)
                txt = [txt, newline];
            end
            txt = [txt, r, newline];
        end
    end
    if isfield(st, 'syntax')
        r = formatSyntax(st, maxLineLength);
        if ~isempty(r)
            txt = [txt, r, newline];
        end
    end
    if isfield(st, 'input_arguments')
        r = formatInputArgs(st, maxLineLength);
        if ~isempty(r)
            txt = [txt, r, newline];
        end
    end
    if isfield(st, 'output_arguments')
        r = formatOutputArgs(st, maxLineLength);
        if ~isempty(r)
            txt = [txt, r, newline];
        end
    end
    if isfield(st, 'examples')
        r = formatExamples(st, maxLineLength);
        if ~isempty(r)
            txt = [txt, r, newline];
        end
    end
    if isfield(st, 'see_also')
        r = formatSeeAlso(st, maxLineLength);
        if ~isempty(r)
            txt = [txt, r, newline];
        end
    end
    if isfield(st, 'keyword')
        r = formatDocumentation(st, maxLineLength);
        if ~isempty(r)
            txt = [txt, r, newline];
        end
    end
end
%=============================================================================
function txt = formatShortDescription(st, maxLineLength)
    if isfield(st, 'keyword')
        txt = [' ', st.keyword, ' - ', st.short_description];
    else
        txt = [' ', st.short_description];        
    end
end
%=============================================================================
function txt = formatSyntax(st, maxLineLength)
    txt = ['   ', _('Syntax: '), newline];
    for i = 1:length(st.syntax)
        txt = [txt, '   ', '   ', st.syntax{i}, newline];
    end
end
%=============================================================================
function txt = formatInputArgs(st, maxLineLength)
    input_args = st.input_arguments;
    nb_input_args = length(input_args);
    if nb_input_args == 0
        txt = '';
        return
    end
    txt = ['   ', _('Input Arguments:'), newline];        
    for i = 1:nb_input_args
        txt = [txt, '   ', '   ',input_args(i).name, ': ', input_args(i).description, newline];
    end
end
%=============================================================================
function txt = formatOutputArgs(st, maxLineLength)
    output_args = st.output_arguments;
    nb_output_args = length(output_args);
    if nb_output_args == 0
        txt = '';
        return
    end
    txt = ['   ', _('Output Arguments:'), newline];
    for i = 1:nb_output_args
        txt = [txt, '   ', '   ', output_args(i).name, ': ', output_args(i).description, newline];
    end
end
%=============================================================================
function txt = formatDescription(st, maxLineLength)
    lines = splitText(limitToTwoSentences(st.description), maxLineLength);
    txt = '';
    for i = 1:length(lines)
        txt = [txt, '   ', lines{i}, newline];
    end
end
%=============================================================================
function txt = formatSeeAlso(st, maxLineLength)
    if isempty(st.see_also)
        txt = '';
        return
    end
    see_also_txt = join(st.see_also, ', ');        
    txt = ['   ', sprintf(_('See also: %s'), see_also_txt{1}), newline];
end
%=============================================================================
function txt = formatExamples(st, maxLineLength)
    examples_txt = join(st.examples, '\n');
    txt = ['   ', sprintf(_('Examples: %s'), examples_txt{1}), newline];
end
%=============================================================================
function txt = formatDocumentation(st, maxLineLength)
    txt = ['   ', sprintf(_('Documentation: %s'), st.keyword), newline];
end
%=============================================================================
function lines = splitText(txt, maxLineLength)
    words = {};
    w = '';
    for k = 1:length(txt)
        ch = txt(k);
        if isspace(ch)
            if ~isempty(w)
                words{end+1} = w;
                w = '';
            end
        else
            w = [w, ch];
        end
    end
    if ~isempty(w)
        words{end+1} = w;
    end

    currentLine = '';
    lines = {};
    for i = 1:length(words)
        word = words{i};
        if isempty(currentLine)
            tentative = word;
        else
            tentative = [currentLine, ' ', word];
        end
        if length(tentative) <= maxLineLength
            currentLine = tentative;
        else
            if ~isempty(currentLine)
                lines{end+1} = currentLine;
            end
            currentLine = word;
        end
    end
    if ~isempty(currentLine)
        lines{end+1} = currentLine;
    end
end
%=============================================================================
function txt = limitToTwoSentences(txt)
    % Extract up to two sentences
    sentences = {};
    current = '';
    truncated = false;
    for k = 1:length(txt)
        ch = txt(k);
        current = [current, ch];
        if ch == '.' || ch == '!' || ch == '?'
            % push sentence (trim to remove leading/trailing spaces)
            sentences{end+1} = strtrim(current);
            current = '';
            if length(sentences) >= 2
                % if there's any non-space character after this point -> truncated
                if k < length(txt)
                    rest = txt(k+1:end);
                    if any(~isspace(rest))
                        truncated = true;
                    end
                end
                break;
            end
        end
    end

    if length(sentences) >= 2
        out = [sentences{1}, ' ', sentences{2}];
    elseif length(sentences) == 1
        out = sentences{1};
    else
        % no sentence terminator found: return trimmed input
        out = strtrim(txt);
    end

    if truncated
        out = [out, ' ...'];
    end

    txt = out;
end
%=============================================================================
