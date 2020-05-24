trait Downcast {
    fn try_downcast<_T>(self) -> Option<_T>.
}

trait DowncastExt {
    fn downcast<_T>(self) -> _T.
    fn is<_T>(self) -> Bool.
}

impl<_S> DowncastExt for _S where _S: Downcast {
    fn downcast<_T>(self) -> _T {
        self:try_downcast():unwrap()
    }


    fn is<_T>(self) -> Bool {
        self:try_downcast:<_T>():is_some()
    }
}