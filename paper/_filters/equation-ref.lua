function Cite(el)
  if #el.citations == 1 then
    local citation = el.citations[1]

    if citation.id:match("^eq%-") then
      citation.mode = pandoc.SuppressAuthor

      return {
        pandoc.Str("("),
        el,
        pandoc.Str(")")
      }
    end
  end

  return el
end
