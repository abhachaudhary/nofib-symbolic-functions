{-
From: David J King [mailto:gnik@dcs.gla.ac.uk]
Sent: Thursday, October 01, 1998 1:37 PM
Subject: Challenge applications -- the ray tracer for spheres


Hello SCOFPIGgers,

At the last SCOFPIG meeting (2nd July, 1998) it was suggested that
we all try our techniques out on the same examples.  There were
two examples that people were reasonably happy with: the Ray Tracer
for Sphere's from the Impala suite; and John O'Donnell's circuit
simulator.

Rather than giving out parallel versions, I think it makes more sense
to give out sequential versions, and then we all do our thing oblivious
to what others might be doing.  Included below is a sequential Haskell
version of the ray tracer for spheres.  Hopefully, it's not too far
removed from the languages people are using.  I'll send the circuit
simulator out later.

Cheers,

David.
--
David J. King                     E-mail: D.J.King@open.ac.uk
Computing Department              URL:    http://mcs.open.ac.uk/djk26/
The Open University               Phone:  (01908) 652056
Milton Keynes, MK7 6AA            Fax:    (01908) 652140
-}
--------------------------------------------------------------------------------
A ray tracer for spheres

This is a Haskell port of the Id version from the Impala
suite URL: http://www.csg.lcs.mit.edu/impala/
16 July, 1998, David J. King (d.j.king@open.ac.uk)

> module Main2 where

> import Control.Monad
> import System.Environment

> epsilon, infinity :: Double
> epsilon  = 1.0e-6
> infinity = 1.0e+20


--------------------------------------------------------------------------------
Convenient vector operations

> type Vector = (Double, Double, Double)

> vecadd, vecsub, vecmult :: Vector -> Vector -> Vector
> vecadd  (x1,y1,z1) (x2,y2,z2) = (x1+x2, y1+y2, z1+z2)
> vecsub  (x1,y1,z1) (x2,y2,z2) = (x1-x2, y1-y2, z1-z2)
> vecmult (x1,y1,z1) (x2,y2,z2) = (x1*x2, y1*y2, z1*z2)

> vecsum :: [Vector] -> Vector
> vecsum = foldr vecadd (0,0,0)

> vecnorm :: Vector -> (Vector, Double)
> vecnorm (x,y,z) = ((x/len, y/len, z/len), len)
>       where len = sqrt (x^2 + y^2 + z^2)

> vecscale :: Vector -> Double -> Vector
> vecscale (x,y,z) a = (a*x, a*y, a*z)

> vecdot :: Vector -> Vector -> Double
> vecdot (x1,y1,z1) (x2,y2,z2) = x1*x2 + y1*y2 + z1*z2

> veccross :: Vector -> Vector -> Vector
> veccross (x1,y1,z1) (x2,y2,z2) = (y1*z2-y2*z1, z1*x2-z2*x1, x1*y2-x2*y1)

> is_zerovector :: Vector -> Bool
> is_zerovector (x,y,z) = x<epsilon && y<epsilon && z<epsilon

--------------------------------------------------------------------------------
type declarations

> data Light = Directional Vector Vector        -- direction, colour
>            | Point Vector Vector              -- position, colour

> lightpos :: Light -> Vector
> lightpos (Point pos col) = pos

> lightdir :: Light -> Vector
> lightdir (Directional dir col) = fst (vecnorm dir)

> lightcolour :: Light -> Vector
> lightcolour (Directional dir col) = col
> lightcolour (Point pos col)       = col


> data Surfspec = Ambient Vector        -- all but specpow default to zero
>               | Diffuse Vector
>               | Specular Vector
>               | Specpow Double        -- default 8
>               | Reflect Double
>               | Transmit Double
>               | Refract Double        -- default 1, like air == no refraction
>               | Body Vector           -- body colour, default (1, 1, 1)


> ambientsurf :: [Surfspec] -> Vector
> ambientsurf ss = head ([ s | Ambient s <- ss] ++ [(0,0,0)])

> diffusesurf :: [Surfspec] -> Vector
> diffusesurf ss = head ([ s | Diffuse s <- ss] ++ [(0,0,0)])

> specularsurf :: [Surfspec] -> Vector
> specularsurf ss = head ([ s | Specular s <- ss] ++ [(0,0,0)])

> specpowsurf :: [Surfspec] -> Double
> specpowsurf ss = head ([ s | Specpow s <- ss] ++ [8])

> reflectsurf :: [Surfspec] -> Double
> reflectsurf ss = head ([ s | Reflect s <- ss] ++ [0])

> transmitsurf :: [Surfspec] -> Double
> transmitsurf ss = head ([ s | Transmit s <- ss] ++ [0])

> refractsurf :: [Surfspec] -> Double
> refractsurf ss = head ([ s | Refract s <- ss] ++ [1])

> bodysurf :: [Surfspec] -> Vector
> bodysurf ss = head ([ s | Body s <- ss] ++ [(1,1,1)])


> data Sphere = Sphere Vector Double [Surfspec] -- pos, radius, surface type

> spheresurf :: Sphere -> [Surfspec]
> spheresurf (Sphere pos rad surf) = surf


--------------------------------------------------------------------------------
Example image with 8 spheres

> lookat, vup :: Vector
> lookat = (0,0,0)
> vup = (0,0,1)

> fov :: Double
> fov = 45

> world :: [Sphere]
> world = testspheres

> redsurf :: [Surfspec]
> redsurf = [Ambient (0.1, 0, 0), Diffuse (0.3, 0, 0),
>            Specular (0.8, 0.4, 0.4), Transmit 0.7]

> greensurf :: [Surfspec]
> greensurf = [Ambient (0, 0.1, 0), Diffuse (0, 0.3, 0),
>              Specular (0.4, 0.8, 0.4)]

> bluesurf :: [Surfspec]
> bluesurf = [Ambient (0, 0, 0.1), Diffuse (0, 0, 0.3),
>             Specular (0.4, 0.4, 0.8)]

standard balls

> s2 :: [Surfspec]
> s2 = [Ambient (0.035, 0.0325, 0.025), Diffuse (0.5, 0.45, 0.35),
>       Specular (0.8, 0.8, 0.8), Specpow 3.0, Reflect 0.5]

> testspheres :: [Sphere]
> testspheres = [(Sphere (0,0,0) 0.5 s2),
>                (Sphere (0.272166,0.272166,0.544331) 0.166667 s2),
>                (Sphere (0.643951,0.172546,0) 0.166667 s2),
>                (Sphere (0.172546,0.643951,0) 0.166667 s2),
>                (Sphere (-0.371785,0.0996195,0.544331) 0.166667 s2),
>                (Sphere (-0.471405,0.471405,0) 0.166667 s2),
>                (Sphere (-0.643951,-0.172546,0) 0.166667 s2),
>                (Sphere (0.0996195,-0.371785,0.544331) 0.166667 s2),
>                (Sphere (-0.172546,-0.643951,0) 0.166667 s2),
>                (Sphere (0.471405,-0.471405,0) 0.166667 s2)]


> testlights :: [Light]
> testlights = [Point (4, 3, 2) (0.288675,0.288675,0.288675),
>               Point (1, -4, 4) (0.288675,0.288675,0.288675),
>               Point (-3,1,5) (0.288675,0.288675,0.288675)]

> lookfrom, background :: Vector
> lookfrom   = (2.1, 1.3, 1.7)
> background = (0.078, 0.361, 0.753)


--------------------------------------------------------------------------------
main routine

> ray :: (Double -> Double) -> ((Double -> Double) -> Double -> Double) -> (Double -> Double) -> Int -> [((Int, Int),Vector)]
> ray sym_tan symFun1 symFun2 winsize = [ ((i,j), f i j) | i<-[0..winsize-1], j<-[0..winsize-1]]
>     where
>       lights = testlights
>       (firstray, scrnx, scrny) = camparams sym_tan lookfrom lookat vup fov (fromIntegral winsize)
>       f i j = tracepixel symFun1 symFun2 world lights (fromIntegral i) (fromIntegral j) firstray scrnx scrny


> dtor :: Double -> Double
> dtor x = x*pi / 180


> camparams :: (Double -> Double)
>           -> Vector -> Vector -> Vector -> Double -> Double
>            -> (Vector, Vector, Vector)
> camparams sym_tan lookfrom lookat vup fov winsize = (firstray, scrnx, scrny)
>     where
>       initfirstray = vecsub lookat lookfrom   -- pre-normalized!
>       (lookdir, dist) = vecnorm initfirstray
>       (scrni, _) = vecnorm (veccross lookdir vup)
>       (scrnj, _) = vecnorm (veccross scrni lookdir)
>       xfov = fov
>       yfov = fov
>       xwinsize = winsize              -- for now, square window
>       ywinsize = winsize
>       magx = 2 * dist * (sym_tan (dtor (xfov/2))) / xwinsize
>       magy = 2 * dist * (sym_tan (dtor (yfov/2))) / ywinsize
>       scrnx = vecscale scrni magx
>       scrny = vecscale scrnj magy
>       firstray = vecsub initfirstray
>                         (vecadd (vecscale scrnx (0.5 * xwinsize))
>                                 (vecscale scrny (0.5 * ywinsize)))

Colour the given pixel

> tracepixel :: ((Double -> Double) -> Double -> Double) -> (Double -> Double) ->  [Sphere] -> [Light] -> Double -> Double -> Vector -> Vector
>            -> Vector -> Vector
> tracepixel f g spheres lights x y firstray scrnx scrny
>               = if hit
>                   then shade f g lights sp pos dir dist (1,1,1)
>                   else background
>     where
>       pos = lookfrom
>       (dir, _) = vecnorm (vecadd (vecadd firstray (vecscale scrnx x))
>                          (vecscale scrny y))
>       (hit, dist, sp) = trace f g spheres pos dir -- pick first intersection


--------------------------------------------------------------------------------
find first intersection point in set of all objects

> trace :: ((Double -> Double) -> Double -> Double) -> (Double -> Double) -> [Sphere] -> Vector -> Vector -> (Bool,Double,Sphere)
> trace g h spheres pos dir
>               = if (null dists)
>                   then (False, infinity, head spheres)        -- missed all
>                   else (True, mindist, sp)            -- pick the smallest one
>     where
>       (mindist, sp) = foldr f (head dists) (tail dists)
>       f (d1,s1) (d2,s2) | d1<d2     = (d1,s1)
>                         | otherwise = (d2,s2)
>     -- make a list of the distances to intersection for each hit object
>       sphmap []     = []
>       sphmap (x:xs) = if is_hit
>                         then (where_hit, x):sphmap xs
>                         else sphmap xs
>               where (is_hit, where_hit) = sphereintersect g h pos dir x
>       dists = sphmap spheres




--------------------------------------------------------------------------------
Complete shader, given set of lights, sphere which was hit, ray which
hit that sphere, and at what distance, return a colour.
Contrib answers "what's the most my result can add to the working
pixel?"  and will abort a reflected or transmitted ray if it gets too
small.

> shade :: ((Double -> Double) -> Double -> Double) -> (Double -> Double) -> [Light] -> Sphere -> Vector -> Vector -> Double -> Vector -> Vector
> shade f g lights sp lookpos dir dist contrib = rcol
>     where
>       hitpos = vecadd lookpos (vecscale dir dist)
>       ambientlight = (1, 1, 1)        -- full contribution as default
>       surf = spheresurf sp
>       amb = vecmult ambientlight (ambientsurf surf)
>       norm = spherenormal hitpos sp
>       refl = vecadd dir (vecscale norm (-2*(vecdot dir norm)))
>       -- diff is diffuse and specular contribution
>       diff = vecsum (map (\l->lightray f g l hitpos norm refl surf) lights)
>       transmitted = transmitsurf surf
>       simple = vecadd amb diff
>       -- calculate transmitted ray; it adds onto "simple"
>       trintensity = vecscale (bodysurf surf) transmitted
>       (is_tir, trcol) = if transmitted < epsilon
>                           then (False, simple)
>                           else transmitray f g lights simple hitpos dir
>                                            index trintensity contrib norm
>                         where index = refractsurf surf
>       -- reflected ray; in case of TIR, add transmitted component
>       reflsurf = vecscale (specularsurf surf) (reflectsurf surf)
>       reflectiv = if (is_tir)
>                     then (vecadd trintensity reflsurf)
>                     else reflsurf
>       rcol = if is_zerovector reflectiv
>                then trcol
>                else reflectray f g hitpos refl lights reflectiv contrib trcol


--------------------------------------------------------------------------------
Transmit a ray through an object

> transmitray :: ((Double -> Double) -> Double -> Double) -> (Double -> Double) -> [Light] -> Vector -> Vector -> Vector -> Double -> Vector
>             -> Vector -> Vector -> (Bool, Vector)
> transmitray f g lights colour pos dir index intens contrib norm
>       = if is_zerovector newcontrib
>           then (False, colour)        -- cutoff
>           else (False, vecadd (vecmult newcol intens) colour)
>       where
>       newcontrib         = vecmult intens contrib
>       (is_tir, newdir)   = refractray index dir norm
>       nearpos            = vecadd pos (vecscale newdir epsilon)
>       (is_hit, dist, sp) = trace f g world nearpos newdir
>       newcol | is_hit    = shade f g lights sp nearpos newdir dist newcontrib
>              | otherwise = background

--------------------------------------------------------------------------------
Reflect a ray from an object

> reflectray :: ((Double -> Double) -> Double -> Double) -> (Double -> Double) -> Vector -> Vector -> [Light] -> Vector -> Vector -> Vector
>            -> Vector
> reflectray f g pos newdir lights intens contrib colour
>       = if is_zerovector newcontrib
>           then colour
>           else vecadd colour (vecmult newcol intens)
>       where
>       newcontrib = vecmult intens contrib
>       nearpos = vecadd pos (vecscale newdir epsilon)
>       (is_hit, dist, sp) = trace f g world nearpos newdir
>       newcol = if is_hit
>                 then shade f g lights sp nearpos newdir dist newcontrib
>                 else background

--------------------------------------------------------------------------------
Refract a ray through a surface (ala Foley, vanDamm, p. 757)
outputs a new direction, and if total internal reflection occurred or
not

> refractray :: Double -> Vector -> Vector -> (Bool,Vector)
> refractray newindex olddir innorm
>               = if disc < 0
>                   then (True, (0,0,0))        -- total internal reflection
>                   else (False, vecadd (vecscale norm t) (vecscale olddir nr))
>       where
>       dotp = -(vecdot olddir innorm)
>       (norm, k, nr) = if dotp < 0
>                         then (vecscale innorm (-1), -dotp, 1/newindex)
>                         else (innorm, dotp, newindex) -- trans. only with air
>       disc = 1 - nr*nr*(1-k*k)
>       t = nr * k - (sqrt disc)

--------------------------------------------------------------------------------
For a given light l, surface hit at pos, with norm and refl components
to incoming ray, figure out which side of the surface the light is on,
and if it's shadowed by another object in the world.  Return light's
contribution to the object's colour

> lightray :: ((Double -> Double) -> Double -> Double) -> (Double -> Double) -> Light -> Vector -> Vector -> Vector -> [Surfspec] -> Vector
> lightray f g l pos norm refl surf =
>      let
>        (ldir, dist) = lightdirection l pos
>        cosangle = vecdot ldir norm      -- lightray is this far off normal
>        (is_inshadow, lcolour) = shadowed f g pos ldir (lightcolour l)
>      in
>        if is_inshadow then (0,0,0)
>        else
>          let
>            diff = diffusesurf surf
>            spow = specpowsurf surf    -- assumed trans is same as refl
>          in
>            if (cosangle <= 0) then    --  opposite side
>              let bodycol = bodysurf surf
>                  cosalpha = -(vecdot refl ldir)
>                  diffcont = vecmult (vecscale diff (-cosangle)) lcolour
>                  speccont = if cosalpha <= 0 then (0,0,0)
>                             else vecmult (vecscale bodycol (cosalpha**spow)) lcolour
>              in vecadd diffcont speccont
>            else
>              let spec = specularsurf surf
>                  cosalpha = vecdot refl ldir
>                                       -- this far off refl ray (for spec)
>                  diffcont = vecmult (vecscale diff cosangle) lcolour
>                  speccont | cosalpha <= 0 = (0,0,0)
>                           | otherwise     = vecmult (vecscale spec (cosalpha**spow)) lcolour
>              in vecadd diffcont speccont




> lightdirection :: Light -> Vector -> (Vector, Double)
> lightdirection (Directional dir col) pt = (fst (vecnorm dir), infinity)
> lightdirection (Point pos col) pt       = vecnorm (vecsub pos pt)

need to offset just a bit

> shadowed :: ((Double -> Double) -> Double -> Double) -> (Double -> Double) -> Vector -> Vector -> a -> (Bool,a)
> shadowed f g pos dir lcolour = if not is_hit
>                             then (False, lcolour)
>                             else (True, lcolour)              -- for now
>     where
>       (is_hit, dist, sp) = trace f g world (vecadd pos (vecscale dir epsilon)) dir

--------------------------------------------------------------------------------
sphere specific items

figure when a ray hits a sphere

Assumes direction vector is normalised!

> sphereintersect :: ((Double -> Double) -> Double -> Double) -> (Double -> Double) -> Vector -> Vector -> Sphere -> (Bool,Double)
> sphereintersect f g pos dir sp = if f g disc < f g 0
>                                then (False, 0)        -- imaginary solns only
>                                else if slo < 0
>                                       then if shi < 0
>                                              then (False, 0)
>                                              else (True, shi)
>                                       else (True, slo)
>       where
>       slo = -bm - (sqrt disc)
>       shi = -bm + (sqrt disc)
>       Sphere spos rad _ = sp
>       m = vecsub pos spos                     -- x-centre
>       m2 = vecdot m m                         -- (x-centre).(x-centre)
>       bm = vecdot m dir                       -- (x-centre).dir
>       disc = bm * bm - m2 + rad * rad         -- discriminant

for shading, need normal at a point

> spherenormal :: Vector-> Sphere -> Vector
> spherenormal pos sp = vecscale (vecsub pos spos) (1/rad)
>     where
>       Sphere spos rad _ = sp


--------------------------------------------------------------------------------

How to compile (with ghc), run (with size 100), and view (with xv):

% ghc -o ray ray_tracer.lhs
% ray 100 > spheres.ppm
% xv spheres.ppm

> --main :: IO ()
> --main = replicateM_ 100 $
> --       getArgs >>= \[winsize_string] ->
> --       run (read winsize_string)
>
> main :: (Double -> Double) -> ((Double -> Double) -> Double -> Double) -> (Double -> Double) -> Int -> Int
> main sym_tan symFun1 symFun2 winsize = run sym_tan symFun1 symFun2 winsize
>
> run :: (Double -> Double) -> ((Double -> Double) -> Double -> Double) -> (Double -> Double) -> Int -> Int
> run sym_tan symFun1 symFun2 winsize = hash (map snd (ray sym_tan symFun1 symFun2 winsize))
>
> hash :: [Vector] -> Int
> hash = foldr (\(r,g,b) acc -> u8 r + u8 g*7 + u8 b*23 + acc*61) 0
>   where
>     u8 = round . (255*)
