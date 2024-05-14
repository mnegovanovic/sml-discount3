structure Discount :>
sig
    val MKD_NOLINKS : int      (* don't do link processing, block <a> tags  *)
    val MKD_NOIMAGE : int        (* don't do image processing, block <img> *)
    val MKD_NOPANTS : int        (* don't run smartypants() *)
    val MKD_NOHTML : int     (* don't allow raw html through AT ALL *)
    val MKD_NORMAL_LISTITEM : int    (* disable github-style checkbox lists *)
    val MKD_TAGTEXT : int        (* process text inside an html tag *)
    val MKD_NO_EXT : int     (* don't allow pseudo-protocols *)
    val MKD_EXPLICITLIST : int   (* don't combine numbered/bulletted lists *)
    val MKD_CDATA : int      (* generate code for xml ![CDATA[...]] *)
    val MKD_NOSUPERSCRIPT : int  (* no A^B *)
    val MKD_STRICT : int     (* conform to Markdown standard as implemented in Markdown.pl *)
    val MKD_NOTABLES : int       (* disallow tables *)
    val MKD_NOSTRIKETHROUGH : int    (* forbid ~~strikethrough~~ *)
    val MKD_1_COMPAT : int       (* compatibility with MarkdownTest_1.0 *)
    val MKD_TOC : int        (* do table-of-contents processing *)
    val MKD_AUTOLINK : int       (* make http://foo.com link even without <>s *)
    val MKD_NOHEADER : int       (* don't process header blocks *)
    val MKD_TABSTOP : int        (* expand tabs to 4 spaces *)
    val MKD_SAFELINK : int       (* paranoid check for link protocol *)
    val MKD_NODIVQUOTE : int     (* forbid >%class% blocks *)
    val MKD_NOALPHALIST : int    (* forbid alphabetic lists *)
    val MKD_EXTRA_FOOTNOTE : int (* enable markdown extra-style footnotes *)
    val MKD_NOSTYLE : int        (* don't extract <style> blocks *)
    val MKD_DLDISCOUNT : int     (* enable discount-style definition lists *)
    val MKD_DLEXTRA : int        (* enable extra-style definition lists *)
    val MKD_FENCEDCODE : int     (* enabled fenced code blocks *)
    val MKD_IDANCHOR : int       (* use id= anchors for TOC links *)
    val MKD_GITHUBTAGS : int     (* allow dash and underscore in element names *)
    val MKD_URLENCODEDANCHOR : int   (* urlencode non-identifier chars instead of replacing with dots *)
    val MKD_LATEX : int      (* handle embedded LaTeX escapes *)
    val MKD_ALT_AS_TITLE : int   (* use alt text as the title if no title is listed *)
    val MKD_NR_FLAGS : int

    type mmiot = MLton.Pointer.t
    type flags = MLton.Pointer.t
    
    val compile: string -> int list -> string
end =
struct
    val MKD_NOLINKS = 0      (* don't do link processing, block <a> tags  *)
    val MKD_NOIMAGE = 1        (* don't do image processing, block <img> *)
    val MKD_NOPANTS = 2        (* don't run smartypants() *)
    val MKD_NOHTML = 3     (* don't allow raw html through AT ALL *)
    val MKD_NORMAL_LISTITEM = 4    (* disable github-style checkbox lists *)
    val MKD_TAGTEXT = 5        (* process text inside an html tag *)
    val MKD_NO_EXT = 6     (* don't allow pseudo-protocols *)
    val MKD_EXPLICITLIST = 7   (* don't combine numbered/bulletted lists *)
    val MKD_CDATA = 8      (* generate code for xml ![CDATA[...]] *)
    val MKD_NOSUPERSCRIPT = 9  (* no A^B *)
    val MKD_STRICT = 10     (* conform to Markdown standard as implemented in Markdown.pl *)
    val MKD_NOTABLES = 11       (* disallow tables *)
    val MKD_NOSTRIKETHROUGH = 12    (* forbid ~~strikethrough~~ *)
    val MKD_1_COMPAT = 13       (* compatibility with MarkdownTest_1.0 *)
    val MKD_TOC = 14        (* do table-of-contents processing *)
    val MKD_AUTOLINK = 15       (* make http://foo.com link even without <>s *)
    val MKD_NOHEADER = 16       (* don't process header blocks *)
    val MKD_TABSTOP = 17        (* expand tabs to 4 spaces *)
    val MKD_SAFELINK = 18       (* paranoid check for link protocol *)
    val MKD_NODIVQUOTE = 19     (* forbid >%class% blocks *)
    val MKD_NOALPHALIST = 20    (* forbid alphabetic lists *)
    val MKD_EXTRA_FOOTNOTE = 21 (* enable markdown extra-style footnotes *)
    val MKD_NOSTYLE = 22        (* don't extract <style> blocks *)
    val MKD_DLDISCOUNT = 23     (* enable discount-style definition lists *)
    val MKD_DLEXTRA = 24        (* enable extra-style definition lists *)
    val MKD_FENCEDCODE = 25     (* enabled fenced code blocks *)
    val MKD_IDANCHOR = 26       (* use id= anchors for TOC links *)
    val MKD_GITHUBTAGS = 27     (* allow dash and underscore in element names *)
    val MKD_URLENCODEDANCHOR = 28   (* urlencode non-identifier chars instead of replacing with dots *)
    val MKD_LATEX = 29      (* handle embedded LaTeX escapes *)
    val MKD_ALT_AS_TITLE = 30   (* use alt text as the title if no title is listed *)
    val MKD_NR_FLAGS = 31
    
    type mmiot = MLton.Pointer.t
    type flags = MLton.Pointer.t
    
    val mkd_flags_ = _import "mkd_flags": unit -> flags;
    val mkd_free_flags_ = _import "mkd_free_flags": flags -> unit;
    val mkd_set_flag_num_ = _import "mkd_set_flag_num": flags * int -> unit;
    
    val mkd_string_ = _import "mkd_string": CString.cstring * int * flags -> mmiot;
    val mkd_compile_ = _import "mkd_compile": mmiot * flags -> int;
    val mkd_document_ = _import "mkd_document": mmiot * CString.t ref -> int;
    val mkd_cleanup_ = _import "mkd_cleanup": mmiot -> unit;


    (*val foldl : ('a * 'b -> 'b) -> 'b -> 'a list -> 'b*)
    fun makeFlags_ (fs: int list): flags =
        List.foldl (fn (f, flags) => (mkd_set_flag_num_ (flags, f); flags)) (mkd_flags_ ()) fs
    
    fun compile (md: string) (flags: int list): string =
        let
            val flags = makeFlags_ flags
            val c_md = CString.fromString md
            val len = String.size md
            val mkds_flags = makeFlags_ [MKD_NOHEADER, MKD_TABSTOP]
            val doc = mkd_string_ (c_md, len, mkds_flags)
            val c_res = mkd_compile_ (doc, flags)
        in
            if c_res = 0 then
                let in
                    mkd_free_flags_ mkds_flags;
                    mkd_free_flags_ flags;
                    mkd_cleanup_ doc;
                    raise Fail "Discount.compile() failed"
                end
            else
                let
                    val res = CString.newNULL ()
                    val _ = mkd_document_ (doc, res)
                    val res  = CString.toStringVal (!res)
                in
                    mkd_free_flags_ mkds_flags;
                    mkd_free_flags_ flags;
                    mkd_cleanup_ doc;
                    res
                end
        end
end

val md = "> #### The quarterly results look great!\n"
^">\n"
^"> - Revenue was off the chart.\n"
^"> - Profits were higher than ever.\n"
^">\n"
^">  *Everything* is going according to **plan**.\n"
val _ = print (Discount.compile md [])

val md = ""
val _ = print (Discount.compile md [])

