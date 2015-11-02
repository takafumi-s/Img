{-
Exifの撮影日時ディレクトリを生成し、画像を整理

GHC v7.8.3
-}

import Control.Applicative
import Control.Monad
import System.Directory
import System.Info (os)
import System.FilePath
import Data.Char (toLower)
import Data.Time
import qualified Data.ByteString.Lazy as B
import Text.Printf (printf)

-- cabal install hsexif
import Graphics.HsExif
-- cabal install split
import Data.List.Split (splitOn)

-- Exif撮影日付用データ型
data DateOriginal = DateOriginal {getYear :: String, getMonth :: String, getDay :: String}

-- コピー先ディレクトリルート
type DestRoot = String

copyWithExifDateOriginal :: FilePath -> DestRoot -> IO()
copyWithExifDateOriginal path destRoot = do
    file <- return path >>= B.readFile
    Just dateOri <- return file >>= getExifDateOriginal
    return dateOri >>= mkDateDir destRoot
    writeFile' (destPath path destRoot dateOri) file
    return ()

-- ファイルからExif撮影日時を取得
getExifDateOriginal :: B.ByteString -> IO (Maybe DateOriginal)
getExifDateOriginal file = do
    rightExif <- parseExif <$> return file
    let exif = (\(Right x) -> x) rightExif
    let dateOri = return exif >>= getDateTimeOriginal >>= Just . toDateOriginal
    return dateOri

-- 日付フォーマット変換
toDateOriginal :: LocalTime -> DateOriginal
toDateOriginal dt =
    let [y, m, d] = splitOn "-" . head . words $ show dt
    in DateOriginal y m d

-- Directory Separetor
ds :: String
ds
    | "linux"   == fmap toLower os = "/"
    -- | "windows" == fmap toLower os = "\\" -- 未検証
    | otherwise = "/"

-- Exifから取得した日付のディレクトリを作成
mkDateDir :: DestRoot -> DateOriginal -> IO()
mkDateDir root (DateOriginal year month day) =  mapM_ mkDir dirs
    where dirs =  tail . scanl1 (++) . map (++ ds) $ [root, year, month, day]

-- コピー先ファイルのパスを生成
destPath :: FilePath -> DestRoot -> DateOriginal -> FilePath
destPath oriPath root (DateOriginal year month day) =
    let destDir = foldl1 (++) $ map (++ "/") [root, year, month, day] 
    in destDir ++ baseName oriPath

-- パスからファイル名を取得
baseName :: FilePath -> String
baseName = last . splitOn ds

-- ディレクトリ作成 (存在チェック有)
mkDir :: FilePath -> IO()
mkDir dirpath = doesDirectoryExist dirpath >>=
    \is -> case is of True  -> return ()
                      False -> createDirectory dirpath

-- ファイル書き込み (存在チェック有)
writeFile' :: FilePath -> B.ByteString -> IO()
writeFile' path contents = doesFileExist path >>=
    \is -> case is of True  -> do
                                printf "File %s is already exist. Not copy\n" path
                                return ()
                      False -> B.writeFile path contents
