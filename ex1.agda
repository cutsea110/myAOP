module ex1 where

open import Data.Nat
open import Data.Product
open import Data.Empty
open import Data.Unit
open import Relation.Nullary
open import Relation.Binary.PropositionalEquality

--
-- Thank you notogawa
--
0≢1 : ∀ {w} {Whatever : Set w} → 0 ≡ 1 → Whatever
0≢1 ()

sucℕ≢0 : ∀ {w} {Whatever : Set w} {n : ℕ} → suc n ≡ 0 → Whatever
sucℕ≢0 ()

ex1-1 : {A : Set}{x : A} → ¬ (Σ[ f ∈ (A → ℕ) ] ((f x ≡ 0 → f x ≡ 1) × (f x ≢ 0 → f x ≡ 0)))
ex1-1 {x = x} (f , proj₁ , proj₂) with f x
... | zero = 0≢1 (proj₁ refl)
... | suc n = sucℕ≢0 (proj₂ (λ ()))
