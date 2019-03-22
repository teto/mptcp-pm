makeMptcpSocket :: IO MptcpSocket
makeMptcpSocket = do
  sock <- makeSocket
-- getFamilyWithMulticasts 
  res <- getFamilyIdS sock mptcpGenlName
  case res of
    Nothing -> error $ "Could not find family " ++ mptcpGenlName
    Just fid -> return  (MptcpSocket sock (trace ("family id"++ show fid ) fid))

