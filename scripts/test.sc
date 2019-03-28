
val fm:FuMorph = new FuMorph(Greek)
val cexString:String = fm.cex(fumm, "fumorph", "temp", "#")
saveString(cexString)
val morphLib = loadLibrary("cex/temp.txt")
val morphColl = morphLib.collectionRepository.get
